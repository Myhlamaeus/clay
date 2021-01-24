{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Clay.Render
( Config (..)
, LayoutMode (..)
, defaultLayoutMode
, logical
, pretty
, compact
, useLogical
, render
, renderLogical
, htmlInline
, putCss
, renderTWith
, renderWith
, renderSelector
, withBanner
, collectAxialSide
)
where

import           Control.Applicative
import           Control.Monad.Writer
import           Control.Monad.Identity (runIdentity)
import           Data.List              (sort)
import           Data.Maybe
import           Data.Text              (Text, pack)
import           Data.Text.Lazy.Builder
import           Prelude                hiding ((**))

import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import qualified Data.Text.Lazy.IO      as Lazy

import           Clay.Common            (browsers)
import           Clay.Directional
import           Clay.Property
import           Clay.Selector
import           Clay.Stylesheet        hiding (Child, query, rule)

import qualified Clay.Stylesheet        as Rule

import Data.These
import Control.Arrow

data LayoutMode
  = LayoutLogical
  | LayoutConvert
    { layoutFlipAxes :: Bool
    , layoutFlipBlock :: Bool
    , layoutFlipInline :: Bool
    }
  deriving (Show, Eq, Ord)

defaultLayoutMode, logical :: LayoutMode
defaultLayoutMode = LayoutConvert False False False
logical = LayoutLogical

data Config = Config
  { indentation    :: Builder
  , newline        :: Builder
  , sep            :: Builder
  , lbrace         :: Builder
  , rbrace         :: Builder
  , finalSemicolon :: Bool
  , warn           :: Bool
  , align          :: Bool
  , banner         :: Bool
  , comments       :: Bool
  , layoutMode     :: LayoutMode
  }

useLogical :: Config -> Config
useLogical cfg = cfg { layoutMode = logical }

-- | Configuration to print to a pretty human readable CSS output.

pretty :: Config
pretty = Config
  { indentation    = "  "
  , newline        = "\n"
  , sep            = " "
  , lbrace         = "{"
  , rbrace         = "}"
  , finalSemicolon = True
  , warn           = True
  , align          = True
  , banner         = True
  , comments       = True
  , layoutMode     = defaultLayoutMode
  }

-- | Configuration to print to a compacted unreadable CSS output.

compact :: Config
compact = Config
  { indentation    = ""
  , newline        = ""
  , sep            = ""
  , lbrace         = "{"
  , rbrace         = "}"
  , finalSemicolon = False
  , warn           = False
  , align          = False
  , banner         = False
  , comments       = False
  , layoutMode     = defaultLayoutMode
  }

-- | Configuration to print to a compacted unreadable CSS output for embedding inline with HTML.

htmlInline :: Config
htmlInline = Config
  { indentation    = ""
  , newline        = ""
  , sep            = ""
  , lbrace         = ""
  , rbrace         = ""
  , finalSemicolon = False
  , warn           = False
  , align          = False
  , banner         = False
  , comments       = False
  , layoutMode     = defaultLayoutMode
  }

-- | Render to CSS using the default configuration (`pretty`) and directly
-- print to the standard output.

putCss :: Css -> IO ()
putCss = Lazy.putStr . render

-- | Render a stylesheet with the default configuration. The pretty printer is
-- used by default.

render :: Css -> Lazy.Text
render = renderWith pretty []

-- | Render a stylesheet with the default configuration using logical values and properties.
-- The pretty printer is used by default.

renderLogical :: Css -> Lazy.Text
renderLogical = renderWith (useLogical pretty) []

-- | Render a stylesheet with a custom configuration and an optional outer
-- scope.

renderTWith :: Monad m => Config -> [App] -> StyleT m () -> m Lazy.Text
renderTWith cfg top css
  = renderBanner cfg
  . toLazyText
  . rules cfg top
  <$> execStyleT css

renderWith :: Config -> [App] -> Css -> Lazy.Text
renderWith cfg top = runIdentity . renderTWith cfg top

-- | Render a single CSS `Selector`.

renderSelector :: Selector -> Lazy.Text
renderSelector = toLazyText . selector compact

-------------------------------------------------------------------------------

renderBanner :: Config -> Lazy.Text -> Lazy.Text
renderBanner cfg
  | banner cfg = withBanner
  | otherwise  = id

withBanner :: Lazy.Text -> Lazy.Text
withBanner = (<> "\n/* Generated with Clay, http://fvisser.nl/clay */")

kframe :: Config -> Keyframes -> Builder
kframe cfg (Keyframes ident xs) =
  foldMap
    ( \(browser, _) ->
      mconcat [ "@" <> fromText browser <> "keyframes "
              , fromText ident
              , newline cfg
              , lbrace cfg
              , newline cfg
              , foldMap (frame cfg) xs
              , rbrace cfg
              , newline cfg
              , newline cfg
              ]
    )
    (unPrefixed browsers)

frame :: Config -> (Double, [Rule]) -> Builder
frame cfg (p, rs) =
  mconcat
    [ fromText (pack (show p))
    , "% "
    , rules cfg [] rs
    ]

query :: Config -> MediaQuery -> [App] -> [Rule] -> Builder
query cfg q sel rs =
  mconcat
    [ mediaQuery q
    , newline cfg
    , lbrace cfg
    , newline cfg
    , rules cfg sel rs
    , rbrace cfg
    , newline cfg
    ]

mediaQuery' :: MediaQuery -> Builder
mediaQuery' (MediaQuery no ty fs) = mconcat
  [ case no of
      Nothing   -> ""
      Just Not  -> "not "
      Just Only -> "only "
  , mediaType ty
  , mconcat ((" and " <>) . feature <$> fs)
  ]

mediaQuery :: MediaQuery -> Builder
mediaQuery mq = mconcat
  [ "@media "
  , mediaQuery' mq
  ]

mediaType :: MediaType -> Builder
mediaType (MediaType (Value v)) = fromText (plain v)

feature :: Feature -> Builder
feature (Feature k mv) =
  case mv of
    Nothing        -> fromText k
    Just (Value v) -> mconcat
      [ "(" , fromText k , ": " , fromText (plain v) , ")" ]
feature (FeatureCustom n) =
  mconcat
    [ "(--"
    , fromText n
    , ")"
    ]

face :: Config -> [Rule] -> Builder
face cfg rs = mconcat
  [ "@font-face"
  , rules cfg [] rs
  ]

rules :: Config -> [App] -> [Rule] -> Builder
rules cfg sel rs = mconcat
  [ rule cfg sel (mapMaybe property rs)
  , ruleAxial cfg sel (mapMaybe propertyAxial rs)
  , ruleDirectional cfg sel (mapMaybe propertyDirectional rs)
  , ruleCornerDirectional cfg sel (mapMaybe propertyCornerDirectional rs)
  , newline cfg
  ,             imp            cfg     `foldMap` mapMaybe imports         rs
  ,             kframe         cfg     `foldMap` mapMaybe kframes         rs
  ,             face           cfg     `foldMap` mapMaybe faces           rs
  ,             customSelector cfg     `foldMap` mapMaybe customSelectors rs
  ,             Clay.Render.customMedia    cfg     `foldMap` mapMaybe customMedias    rs
  , (\(a, b) -> rules  cfg (a : sel) b) `foldMap` mapMaybe nested          rs
  , (\(a, b) -> query  cfg  a   sel  b) `foldMap` mapMaybe queries         rs
  ]
  where property                  (Property m k v)                  = Just (m, k, v)
        property                  _                                 = Nothing
        propertyAxial             (PropertyAxial m k v)             = Just       (m, k, v)
        propertyAxial             _                                 = Nothing
        propertyDirectional       (PropertyDirectional m k v)       = Just       (m, k, v)
        propertyDirectional       _                                 = Nothing
        propertyCornerDirectional (PropertyCornerDirectional m k v) = Just (m, k, v)
        propertyCornerDirectional _                                 = Nothing
        nested                    (Nested a ns)                     = Just       (a, ns)
        nested                    _                                 = Nothing
        queries                   (Query q ns )                     = Just       (q, ns)
        queries                   _                                 = Nothing
        kframes                   (Keyframe fs)                     = Just fs
        kframes                   _                                 = Nothing
        faces                     (Face ns    )                     = Just ns
        faces                     _                                 = Nothing
        imports                   (Import i   )                     = Just i
        imports                   _                                 = Nothing
        customSelectors           (CustomSelector n s)              = Just       (n, s)
        customSelectors           _                                 = Nothing
        customMedias              (CustomMedia n mq)                = Just       (n, mq)
        customMedias              _                                 = Nothing

imp :: Config -> Text -> Builder
imp cfg t =
  mconcat
    [ "@import url("
    , fromText t
    , ");"
    , newline cfg ]

-- | A key-value pair with associated comment.
type KeyVal = ([Modifier], Key (), Value)
type PartedKeyVal a = ([Modifier], PartedKey (), a)

rule' :: Config -> [App] -> [Representation] -> Builder
rule' _   _   []    = mempty
rule' cfg sel props =
  mconcat
    [ selector cfg (merger sel)
    , newline cfg
    , lbrace cfg
    , newline cfg
    , properties cfg props
    , rbrace cfg
    , newline cfg
    ]

rule :: Config -> [App] -> [KeyVal] -> Builder
rule cfg sel props = rule' cfg sel $ collect =<< props

ruleAxial :: Config -> [App] -> [PartedKeyVal (Axial Value)] -> Builder
ruleAxial cfg sel props = rule' cfg sel $ collectAxial (layoutMode cfg) =<< props

ruleDirectional :: Config -> [App] -> [PartedKeyVal (Directional Value)] -> Builder
ruleDirectional cfg sel props = rule' cfg sel $ collectDirectional (layoutMode cfg) =<< props

ruleCornerDirectional :: Config -> [App] -> [PartedKeyVal (CornerDirectional (Value, Value))] -> Builder
ruleCornerDirectional cfg sel props = rule' cfg sel $ collectCornerDirectional (layoutMode cfg) =<< props



merger :: [App] -> Selector
merger []     = "" -- error "this should be fixed!"
merger (x:xs) =
  case x of
    Rule.Child s -> case xs of [] -> s; _  -> merger xs |> s
    Sub        s -> case xs of [] -> s; _  -> merger xs ** s
    Root       s -> s ** merger xs
    Pop        i -> merger (drop i (x:xs))
    Self       f -> case xs of [] -> star `with` f; _ -> merger xs `with` f

data Representation
  = Warning Text
  | KeyValRep [Modifier] Text Text
  deriving (Show)

keys :: [Representation] -> [Text]
keys = mapMaybe f
  where
    f (KeyValRep _ k _) = Just k
    f _                 = Nothing

collect :: KeyVal -> [Representation]
collect (ms, Key ky, Value vl) = case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> [prop k v]
    ( Prefixed ks , Plain    v  ) -> flip map ks $ \(p, k) -> prop (p <> k) v
    ( Plain    k  , Prefixed vs ) -> flip map vs $ \(p, v) -> prop k (p <> v)
    ( Prefixed ks , Prefixed vs ) -> flip map ks $ \(p, k) -> (Warning (p <> k) `maybe` (prop (p <> k) . mappend p)) (lookup p vs)
  where prop k v = KeyValRep ms k v

properties :: Config -> [Representation] -> Builder
properties cfg xs =
  let width     = 1 + maximum (Text.length <$> keys xs)
      ind       = indentation cfg
      new       = newline cfg
      finalSemi = if finalSemicolon cfg then ";" else ""
   in (<> new) $ (<> finalSemi) $ intercalate (";" <> new) $ flip map xs $ \p ->
        case p of
          Warning w -> if warn cfg
                    then ind <> "/* no value for " <> fromText w <> " */" <> new
                    else mempty
          KeyValRep ms k v ->
            let pad = if align cfg
                      then fromText (Text.replicate (width - Text.length k) " ")
                      else ""
                imptant = maybe "" ((" " <>) . fromText) . foldMap _Important $ ms
                comm = case (foldMap _Comment ms, comments cfg) of
                  (Just c, True) -> " /* " <> fromText (unCommentText c) <> " */"
                  _              -> mempty
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v, imptant, comm]

fromLogical :: IsDirectional dir => LayoutMode -> dir a -> dir a
fromLogical LayoutLogical a = a
fromLogical (LayoutConvert False False False) a = a
fromLogical (LayoutConvert True b i) a = fromLogical (LayoutConvert False b i) $ flipAxes a
fromLogical (LayoutConvert f True i) a = fromLogical (LayoutConvert f False i) $ flipOverInline a
fromLogical (LayoutConvert f b True) a = fromLogical (LayoutConvert f b False) $ flipOverBlock a

data Axis = AxisBlock | AxisInline

collectAxialSide :: LayoutMode -> PartedKeyVal (Axial Value) -> [Representation]
collectAxialSide l (ms, k@(PartedKey _ k'), v) = go =<< splitAxes (fromLogical l v)
  where
    go (AxisBlock, b) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "block") k, b)
      _ ->  case k' of
        That "size" -> collect (ms, "height", b)
        These "min" "size" -> collect (ms, "min-height", b)
        These "max" "size" -> collect (ms, "max-height", b)
        _ -> collectDirectional l (ms, k, toDirectional $ block' b)
    go (AxisInline, i) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "inline") k, i)
      _ ->  case k' of
        That "size" -> collect (ms, "width", i)
        These "min" "size" -> collect (ms, "min-width", i)
        These "max" "size" -> collect (ms, "max-width", i)
        _ -> collectDirectional l (ms, k, toDirectional $ inline' i)
    splitAxes = mergeTheseWith (pure . (AxisBlock, )) (pure . (AxisInline, )) (<>) . unAxial

collectAxial :: LayoutMode -> PartedKeyVal (Axial Value) -> [Representation]
collectAxial l (ms, k, v) | l /= LayoutLogical && l /= (LayoutConvert False False False) = collectAxial (LayoutConvert False False False) (ms, k, fromLogical l v)
collectAxial l (ms_, k_, v_) = collectAxial' l (ms_, k_, fromLogical l v_)
  where
  -- Inset doesn't have physical axial shorthands
  collectAxial' (LayoutConvert _ _ _) (ms, k@(PartedKey _ (This "inset")), v) = collectDirectional l (ms, k, toDirectional v)
  -- Size properties don't have a shorthand
  collectAxial' _ (ms, k@(PartedKey _ (That "size")),      v) = collectAxialSide l (ms, k, v)
  collectAxial' _ (ms, k@(PartedKey _ (These _ "size")),   v) = collectAxialSide l (ms, k, v)
  -- Border has no shorthand for specifying different values per axis (ie no "border: 1px solid white 1px solid white")
  collectAxial' _ (ms, k@(PartedKey _ (This "border")),    v@(Axial (These b i))) | b == i     = collect (ms, partedToKey Nothing k, b)
                                                                                | otherwise = collectAxialSide l (ms, k, v)
  -- Use the appropriate shorthand and prefix values of logical shorthands with "logical "
  collectAxial' _ (ms, k@(PartedKey _ k'), Axial (These b i)) | b == i     = collect (ms, partedToKey Nothing k, b)
                                             | otherwise = collect (ms, partedToKey Nothing k, noCommas $ case l of
                                                                      LayoutLogical ->  case k' of
                                                                        -- Logical inset is missing the "logical" keyword for whatever reason
                                                                        This "inset" -> [b, i]
                                                                        _ ->            ["logical", b, i]
                                                                      _ -> [b, i]
                                                                   )
  collectAxial' _ (ms, k, v) = collectAxialSide l (ms, k, v)

data Dir = DirStart Axis | DirEnd Axis

collectDirectionalSeparate :: LayoutMode -> PartedKeyVal (Directional Value) -> [Representation]
collectDirectionalSeparate l (ms, k@(PartedKey _ k'), v) = go =<< splitDir (fromLogical l v)
  where
    go (DirStart AxisBlock, bs) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "block-start") k, bs)
      _ ->  case k' of
        This "inset" -> collect (ms, "top", bs)
        _ -> collect (ms, partedToKey (Just "top") k, bs)
    go (DirStart AxisInline, is) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "inline-start") k, is)
      _ ->  case k' of
        This "inset" -> collect (ms, "left", is)
        _ -> collect (ms, partedToKey (Just "left") k, is)
    go (DirEnd AxisBlock, be) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "block-end") k, be)
      _ ->  case k' of
        This "inset" -> collect (ms, "bottom", be)
        _ -> collect (ms, partedToKey (Just "bottom") k, be)
    go (DirEnd AxisInline, ie) = case l of
      LayoutLogical -> collect (ms, partedToKey (Just "inline-end") k, ie)
      _ ->  case k' of
        This "inset" -> collect (ms, "right", ie)
        _ -> collect (ms, partedToKey (Just "right") k, ie)
    splitAxes = mergeTheseWith (pure . (AxisBlock, )) (pure . (AxisInline, )) (<>) . unAxial
    splitDir = mergeTheseWith (fmap (first DirStart) . splitAxes) (fmap (first DirEnd) . splitAxes) (<>) . unDirectional

collectDirectional :: LayoutMode -> PartedKeyVal (Directional Value) -> [Representation]
collectDirectional l (ms, k, v) | l /= LayoutLogical && l /= (LayoutConvert False False False) = collectDirectional (LayoutConvert False False False) (ms, k, fromLogical l v)
collectDirectional l (ms_, k_, v_) = collectDirectional' l (ms_, k_, fromLogical l v_)
  where
    -- There's no shorthand for top/right/bottom/left
    collectDirectional' (LayoutConvert _ _ _) (ms, k@(PartedKey _ (This "inset")), v) = collectDirectionalSeparate l (ms, k, v)
    collectDirectional' _ (ms, k@(PartedKey _ k'), v@(Directional (These (Axial (These bs is)) (Axial (These be ie)))))
      -- If there is no shorthand available collectAxial splits up the axes and recurses into collectDirectional for each of them;
      -- this is safe, as it wouldn't match 'Axial (These s e)' on either side
      | bs == be && is == ie = collectAxial l (ms, k, eachAxis bs is)
      | k' == This "border"     = collectDirectionalSeparate l (ms, k, v)
      | otherwise = collect (ms, partedToKey Nothing k, noCommas $ case l of
                                LayoutLogical -> case k' of
                                  -- Logical inset is missing the "logical" keyword for whatever reason
                                  This "inset" -> [bs, is, be, ie]
                                  _ ->            ["logical", bs, is, be, ie]
                                -- Physical layouts have inline-start and inline-end (left and right) flipped in the shorthand
                                _ -> [bs, ie, be, is]
                            )
    collectDirectional' _ (ms, k, v@(Directional (These (Axial (This bs)) (Axial (This be)))))
      -- We can only safely recurse into collectAxial if there's a shorthand available
      | bs == be && l == LayoutLogical = collectAxial l (ms, k, block' bs)
      | otherwise = collectDirectionalSeparate l (ms, k, v)
    collectDirectional' _ (ms, k, Directional (These (Axial (This bs)) (Axial (These be ie)))) = (collectDirectional' l . (ms, k, )) =<< [eachSide (block' bs) (block' be), inlineEnd ie]
    collectDirectional' LayoutLogical (ms, k, v@(Directional (These (Axial (That is)) (Axial (That ie)))))
      -- We can only safely recurse into collectAxial if there's a shorthand available
      | is == ie && l == LayoutLogical = collectAxial l (ms, k, inline' is)
      | otherwise = collectDirectionalSeparate l (ms, k, v)
    -- There are no shorthands for three directions
    collectDirectional' _ (ms, k, Directional (These (Axial (These bs is)) (Axial (That ie)))) = (collectDirectional' l . (ms, k, )) =<< [blockStart bs, eachSide (inline' is) (inline' ie)]
    collectDirectional' _ (ms, k, v) = collectDirectionalSeparate l (ms, k, v)

collectCornerDirectionalSeparate :: LayoutMode -> PartedKeyVal (CornerDirectional (Value, Value)) -> [Representation]
collectCornerDirectionalSeparate l (ms, k, CornerDirectional v) = go =<< splitDir (fmap joinValue $ fromLogical l v)
  where
    joinValue (ss, ss') | ss == ss' = ss
                        | otherwise = noCommas [ss, "/", ss']
    go (dir, v') = collect (ms, partedToKey (Just $ dirName dir) k, v')
    dirName (DirStart AxisBlock) = "start-start"
    dirName (DirStart AxisInline) = "end-start"
    dirName (DirEnd AxisBlock) = "end-end"
    dirName (DirEnd AxisInline) = "end-end"
    splitAxes = mergeTheseWith (pure . (AxisBlock, )) (pure . (AxisInline, )) (<>) . unAxial
    splitDir = mergeTheseWith (fmap (first DirStart) . splitAxes) (fmap (first DirEnd) . splitAxes) (<>) . unDirectional

collectCornerDirectional :: LayoutMode -> PartedKeyVal (CornerDirectional (Value, Value)) -> [Representation]
collectCornerDirectional l (ms, k, v) | l /= LayoutLogical && l /= (LayoutConvert False False False) = collectCornerDirectional (LayoutConvert False False False) (ms, k, fromLogical l v)
collectCornerDirectional l (ms_, k_, v_) = collectCornerDirectional' (ms_, k_, fromLogical l v_)
  where
    collectCornerDirectional' (ms, k, CornerDirectional (Directional (These (Axial (These (ss, ss') (es, es'))) (Axial (These (ee, ee') (se, se'))))))
      | all (== ss) [ss', es, es', ee, ee', se, se'] = collect (ms, partedToKey Nothing k, ss)
      | all (== ss) [es, ee, se] && all (== ss') [es', ee', se'] = collect (ms, partedToKey Nothing k, noCommas [ss, "/", ss'])
      -- There's no logical variant for the remaining shorthands and it would be a pain to write them all down so we'll just ignore them
      | otherwise = (collectCornerDirectional' . (ms, k, )) =<< [startStart (ss, ss'), endStart (es, es'), endEnd (ee, ee'), startEnd (se, se')]
    collectCornerDirectional' (ms, k, v) = collectCornerDirectionalSeparate l (ms, k, v)

selector :: Config -> Selector -> Builder
selector Config { lbrace = "", rbrace = "" } = rec
  where rec _ = ""
selector cfg = intercalate ("," <> newline cfg) . rec
  where rec (In (SelectorF (Refinement ft) p)) = (<> foldMap predicate (sort ft)) <$>
          case p of
            Star           -> if null ft then ["*"] else [""]
            Elem t         -> [fromText t]
            Child      a b -> ins " > " <$> rec a <*> rec b
            Deep       a b -> ins " "   <$> rec a <*> rec b
            Adjacent   a b -> ins " + " <$> rec a <*> rec b
            Sibling    a b -> ins " ~ " <$> rec a <*> rec b
            Combined   a b -> rec a ++ rec b
          where ins s a b = a <> s <> b

customSelector :: Config -> (Text, Selector) -> Builder
customSelector cfg (n, s) =
  mconcat
    [ "@custom-selector :--"
    , fromText n
    , " "
    , selector cfg s
    , ";"
    , newline cfg ]

customMedia :: Config -> (Text, CustomMediaQuery) -> Builder
customMedia cfg (n, mq) =
  mconcat
    [ "@custom-media --"
    , fromText n
    , " "
    , mq'
    , ";"
    , newline cfg ]
  where
    mq' = case mq of
      CustomMediaQueryList mqs -> foldMap mediaQuery' mqs
      CustomMediaQueryBool True -> "true"
      CustomMediaQueryBool False -> "false"

predicate :: Predicate -> Builder
predicate ft = mconcat $
  case ft of
    Id           a   -> [ "#" , fromText a                                             ]
    Class        a   -> [ "." , fromText a                                             ]
    Attr         a   -> [ "[" , fromText a,                     "]"                    ]
    AttrVal      a v -> [ "[" , fromText a,  "='", fromText v, "']"                    ]
    AttrBegins   a v -> [ "[" , fromText a, "^='", fromText v, "']"                    ]
    AttrEnds     a v -> [ "[" , fromText a, "$='", fromText v, "']"                    ]
    AttrContains a v -> [ "[" , fromText a, "*='", fromText v, "']"                    ]
    AttrSpace    a v -> [ "[" , fromText a, "~='", fromText v, "']"                    ]
    AttrHyph     a v -> [ "[" , fromText a, "|='", fromText v, "']"                    ]
    Pseudo       a   -> [ ":" , fromText a                                             ]
    PseudoFunc   a p -> [ ":" , fromText a, "(", intercalate "," (map fromText p), ")" ]
    PseudoElem   a   -> [ "::", fromText a                                             ]
    Custom       a   -> [ ":--", fromText a                                             ]
