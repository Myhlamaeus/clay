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
, renderWith
, renderSelector
, withBanner
)
where

import           Control.Applicative
import           Control.Monad.Writer
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

import Clay.Internal


data LayoutMode
  = LayoutLogical
  | LayoutConvert
    { layoutFlipAxes :: Bool
    , layoutFlipBlock :: Bool
    , layoutFlipInline :: Bool
    }

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

renderWith :: Config -> [App] -> Css -> Lazy.Text
renderWith cfg top
  = renderBanner cfg
  . toLazyText
  . rules cfg top
  . runS

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

mediaQuery :: MediaQuery -> Builder
mediaQuery (MediaQuery no ty fs) = mconcat
  [ "@media "
  , case no of
      Nothing   -> ""
      Just Not  -> "not "
      Just Only -> "only "
  , mediaType ty
  , mconcat ((" and " <>) . feature <$> fs)
  ]

mediaType :: MediaType -> Builder
mediaType (MediaType (Value v)) = fromText (plain v)

feature :: Feature -> Builder
feature (Feature k mv) =
  case mv of
    Nothing        -> fromText k
    Just (Value v) -> mconcat
      [ "(" , fromText k , ": " , fromText (plain v) , ")" ]

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
  , newline cfg
  ,             imp    cfg              `foldMap` mapMaybe imports rs
  ,             kframe cfg              `foldMap` mapMaybe kframes rs
  ,             face   cfg              `foldMap` mapMaybe faces   rs
  , (\(a, b) -> rules  cfg (a : sel) b) `foldMap` mapMaybe nested  rs
  , (\(a, b) -> query  cfg  a   sel  b) `foldMap` mapMaybe queries rs
  ]
  where property            (Property m k v)            = Just (m, k, v)
        property            _                           = Nothing
        propertyAxial       (PropertyAxial m k v)       = Just (m, k, v)
        propertyAxial       _                           = Nothing
        propertyDirectional (PropertyDirectional m k v) = Just (m, k, v)
        propertyDirectional _                           = Nothing
        nested              (Nested a ns)               = Just (a, ns)
        nested              _                           = Nothing
        queries             (Query q ns )               = Just (q, ns)
        queries             _                           = Nothing
        kframes             (Keyframe fs)               = Just fs
        kframes             _                           = Nothing
        faces               (Face ns    )               = Just ns
        faces               _                           = Nothing
        imports             (Import i   )               = Just i
        imports             _                           = Nothing

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

collectAxial :: LayoutMode -> PartedKeyVal (Axial Value) -> [Representation]
collectAxial l (ms, PartedKey pfx Nothing "inset" Nothing, ax) = case (l, fromLogical l ax) of
  (LayoutLogical, AxialBoth a) -> collect (ms, logicalShorthand, a)
  (LayoutLogical, AxialEach b i) -> collect (ms, logicalShorthand, noCommas [b, i])
  (LayoutLogical, ax') -> case axialToTuple ax' of
    (Just b, Just i) -> collect (ms, logicalShorthand, noCommas [b, i])
    (b, i) -> join $ zipWith (\k v -> foldMap (collect . (ms, k, )) v) logicalKeys [b, i]
  (_, ax') -> case axialToTuple ax' of
    (b, i) -> join $ zipWith (\k v -> foldMap (collect . (ms, k, )) v) physicalKeys [b, i, b, i]
  where
    ky n = case pfx of
      [] -> Key $ Plain n
      pfx' -> Key $ Prefixed $ (, n) <$> pfx'
    logicalShorthand = ky "inset"
    logicalKeys = ky . ("inset-" <>) <$> ["block", "inline"]
    physicalKeys = ky <$> ["top", "right", "bottom", "left"]
collectAxial l (ms, PartedKey pfx xpfx k sfx, ax) = case (l, k, fromLogical l ax) of
  (LayoutLogical, "size", ax') -> case axialToTuple ax' of
    (b, i) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) logicalKeys [b, i]
  (_, "size", ax') -> case axialToTuple ax' of
    (b, i) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) physicalKeys [b, i]
  (_, _, AxialBoth a) -> collect (ms, shorthand, a)
  (LayoutLogical, "border", AxialEach b i) -> join $ zipWith (\k' -> collect . (ms, k', )) logicalKeys [b, i]
  (LayoutLogical, _, AxialEach b i) -> collect (ms, shorthand, noCommas ["logical", b, i])
  (LayoutLogical, _, ax') -> case axialToTuple ax' of
    (b, i) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) logicalKeys [b, i]
  (_, "border", AxialEach b i) -> join $ zipWith (\k' -> collect . (ms, k', )) physicalKeys [b, i, b, i]
  (_, _, AxialEach b i) -> collect (ms, shorthand, noCommas [b, i])
  (_, _, ax') -> case axialToTuple ax' of
    (b, i) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) physicalKeys [b, i, b, i]
  where
    xprefix = (maybe "" (<> "-") xpfx <>)
    ky n = case pfx of
      [] -> Key $ Plain $ xprefix n
      pfx' -> Key $ Prefixed $ (, xprefix n) <$> pfx'
    shorthand = ky k
    suffix = (<> maybe "" ("-" <>) sfx)
    logicalKeys = case k of
      "size" -> ky <$> ["block-size", "inline-size"]
      k' -> ky . suffix . ((k' <> "-") <>) <$> ["block", "inline"]
    physicalKeys = case k of
      "size" -> ky <$> ["height", "width"]
      k' -> ky . suffix . ((k' <> "-") <>) <$> ["top", "right", "bottom", "left"]

collectDirectional :: LayoutMode -> PartedKeyVal (Directional Value) -> [Representation]
collectDirectional l (ms, PartedKey pfx Nothing "inset" Nothing, dir) = case (l, directionalToTuple $ fromLogical l dir) of
  (LayoutLogical, (Just bs, Just is, Just be, Just ie)) -> collect (ms, logicalShorthand, noCommas [bs, is, be, ie])
  (LayoutLogical, (bs, is, be, ie)) -> join $ zipWith (\k -> foldMap (collect . (ms, k, ))) logicalKeys [bs, is, be, ie]
  -- inline-start and inline-end are flipped in physical
  (_, (bs, is, be, ie)) -> join $ zipWith (\k v -> foldMap (collect . (ms, k, )) v) physicalKeys [bs, ie, be, is]
  where
    ky n = case pfx of
      [] -> Key $ Plain n
      pfx' -> Key $ Prefixed $ (, n) <$> pfx'
    logicalShorthand = ky "inset"
    logicalKeys = ky . ("inset-" <>) <$> ["block-start", "inline-start", "block-end", "inline-end"]
    physicalKeys = ky <$> ["top", "right", "bottom", "left"]
collectDirectional l (ms, PartedKey pfx xpfx k sfx, dir) = case (l, k, directionalToTuple $ fromLogical l dir) of
  (LayoutLogical, "border", (Just bs, Just is, Just be, Just ie)) -> join $ zipWith (\k' -> collect . (ms, k', )) logicalKeys [bs, is, be, ie]
  (LayoutLogical, _, (Just bs, Just is, Just be, Just ie)) -> collect (ms, shorthand, noCommas ["logical", bs, is, be, ie])
  (LayoutLogical, _, (bs, is, be, ie)) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) logicalKeys [bs, is, be, ie]
  -- inline-start and inline-end are flipped in physical
  (LayoutConvert _ _ _, "border", (Just bs, Just is, Just be, Just ie)) -> join $ zipWith (\k' -> collect . (ms, k', )) physicalKeys [bs, ie, be, is]
  (_, _, (Just bs, Just is, Just be, Just ie)) -> collect (ms, shorthand, noCommas [bs, ie, be, is])
  (_, _, (bs, is, be, ie)) -> join $ zipWith (\k' -> foldMap (collect . (ms, k', ))) physicalKeys [bs, ie, be, is]
  where
    xprefix = (maybe "" (<> "-") xpfx <>)
    ky n = case pfx of
      [] -> Key $ Plain $ xprefix n
      pfx' -> Key $ Prefixed $ (, xprefix n) <$> pfx'
    suffix = (<> maybe "" ("-" <>) sfx)
    shorthand = ky $ suffix k
    logicalKeys = ky . suffix . ((k <> "-") <>) <$> ["block-start", "inline-start", "block-end", "inline-end"]
    -- inline-start and inline-end are flipped in physical
    physicalKeys = ky . suffix . ((k <> "-") <>) <$> ["top", "right", "bottom", "left"]

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
