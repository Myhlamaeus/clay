{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Clay.Stylesheet where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Writer (censor, WriterT(runWriterT),  execWriterT, tell)
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text (Text)

import Clay.Selector hiding (Child)
import Clay.Property
import Clay.Common
import Clay.Directional

import Clay.Internal

-------------------------------------------------------------------------------

newtype MediaType = MediaType Value
  deriving (Val, Other, Show, All)

data NotOrOnly = Not | Only
  deriving Show

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]
  deriving Show

data CustomMediaQuery
  = CustomMediaQueryList [MediaQuery]
  | CustomMediaQueryBool Bool
  deriving Show

data Feature
  = Feature       Text (Maybe Value)
  | FeatureCustom Text
  deriving Show

newtype CommentText = CommentText { unCommentText :: Text }
  deriving (Show, IsString, Semigroup, Monoid)

data Modifier
  = Important
  | Comment CommentText
  deriving (Show)

_Important :: Modifier -> Maybe Text
_Important Important   = Just "!important"
_Important (Comment _) = Nothing

_Comment :: Modifier -> Maybe CommentText
_Comment (Comment c) = Just c
_Comment Important   = Nothing

-------------------------------------------------------------------------------

data App
  = Self   Refinement
  | Root   Selector
  | Pop    Int
  | Child  Selector
  | Sub    Selector
  deriving Show

data Keyframes = Keyframes Text [(Double, [Rule])]
  deriving Show

class IsDirectional dir where
  keyDirectional :: Style m => Val a => PartedKey a -> dir a -> m ()
  bothToEach :: dir a -> dir a
  flipAxes :: dir a -> dir a
  flipOverBlock :: dir a -> dir a
  flipOverInline :: dir a -> dir a

instance IsDirectional Axial where
  keyDirectional = keyAxial
  bothToEach (AxialBoth a) = AxialEach a a
  bothToEach a = a
  flipAxes (AxialEach b i) = AxialEach i b
  flipAxes (AxialBlock b) = AxialInline b
  flipAxes (AxialInline i) = AxialBlock i
  flipAxes ax = ax
  flipOverBlock = id
  flipOverInline = id

instance IsDirectional Directional where
  keyDirectional k v = rule $ PropertyDirectional [] (castParted k) (value <$> v)
  bothToEach (DirectionalEach s e) = DirectionalEach (bothToEach s) (bothToEach e)
  bothToEach (DirectionalStart s) = DirectionalStart (bothToEach s)
  bothToEach (DirectionalEnd e) = DirectionalEnd (bothToEach e)
  flipAxes (DirectionalEach s e) = DirectionalEach (flipAxes s) (flipAxes e)
  flipAxes (DirectionalStart s) = DirectionalStart (flipAxes s)
  flipAxes (DirectionalEnd e) = DirectionalEnd (flipAxes e)
  -- bs is be ie -> bs ie be is
  flipOverBlock = unsafeModifyDirectionalAsTuple $ \(bs, is, be, ie) -> (bs, ie, be, is)
  -- bs is be ie -> be is bs ie
  flipOverInline = unsafeModifyDirectionalAsTuple $ \(bs, is, be, ie) -> (be, is, bs, ie)

type BorderEntry = (Maybe Value, Maybe Value, Maybe Value)

data Rule
  = Property             [Modifier] (Key ())       Value
  | PropertyAxial        [Modifier] (PartedKey ()) (Axial Value)
  | PropertyDirectional  [Modifier] (PartedKey ()) (Directional Value)
  | Nested         App        [Rule]
  | Query          MediaQuery [Rule]
  | Face                      [Rule]
  | Keyframe       Keyframes
  | Import         Text
  | CustomSelector Text       Selector
  | CustomMedia    Text       CustomMediaQuery
  deriving Show

newtype StyleT m a = StyleT { unStyleT :: WriterT [Rule] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadFail, MonadIO, Alternative, MonadPlus)

class Monad m => Style m where
  rule :: Rule -> m ()
  default rule :: ( MonadTrans t
                 , m ~ t m'
                 , Style m'
                 )
               => Rule -> m ()
  rule = lift . rule

modifyRules :: Monad m => ([Rule] -> [Rule]) -> StyleT m a -> StyleT m a
modifyRules f (StyleT a) = StyleT $ censor f a

instance Monad m => Style (StyleT m) where
  rule = StyleT . tell . pure

instance Style m => Style (ContT r m)
instance Style m => Style (ExceptT e m)
instance (Style m, Monoid w) => Style (RWSLazy.RWST r w s m)
instance (Style m, Monoid w) => Style (RWSStrict.RWST r w s m)
instance Style m => Style (ReaderT r m)
instance Style m => Style (StateLazy.StateT s m)
instance Style m => Style (StateStrict.StateT s m)
instance (Style m, Monoid w) => Style (WriterT w m)
instance (Style m, Monoid w) => Style (WriterStrict.WriterT w m)

type StyleM = StyleT Identity

runStyleT :: Monad m => StyleT m a -> m (a, [Rule])
runStyleT (StyleT a) = runWriterT a

execStyleT :: Monad m => StyleT m a -> m [Rule]
execStyleT (StyleT a) = execWriterT a

runS :: StyleT Identity () -> [Rule]
runS = runIdentity . execStyleT

-- | The `Css` context is used to collect style rules which are mappings from
-- selectors to style properties. The `Css` type is a computation in the
-- `StyleM` monad that just collects and doesn't return anything.

type Css = StyleM ()

instance Semigroup Css where
  (<>) = liftA2 (<>)

instance Monoid Css where
  mempty = pure ()
  mappend = (<>)

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value. The value can be any type that is in the `Val' typeclass, with other
-- words: can be converted to a `Value`.

key :: (Val a, Style m) => Key a -> a -> m ()
key k v = rule $ Property [] (cast k) (value v)

keyAxial :: (Val a, Style m) => PartedKey a -> Axial a -> m ()
keyAxial k v = rule $ PropertyAxial [] (castParted k) (value <$> v)

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value, like `key` but use a `Prefixed` key.

prefixed :: (Val a, Style m) => Prefixed -> a -> m ()
prefixed xs = key (Key xs)

infix 4 -:

-- | The colon operator can be used to add style rules to the current context
-- for which there is no embedded version available. Both the key and the value
-- are plain text values and rendered as is to the output CSS.

(-:) :: Style m => Key Text -> Text -> m ()
(-:) = key

-------------------------------------------------------------------------------

infixr 5 <?
infixr 5 ?
infixr 5 &

nest :: Monad m => App -> StyleT m () -> StyleT m ()
nest app = modifyRules (pure . Nested app)

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `deep`.

(?) :: Monad m => Selector -> StyleT m () -> StyleT m ()
(?) sel = nest (Sub sel)

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `|>`.

(<?) :: Monad m => Selector -> StyleT m () -> StyleT m ()
(<?) sel = nest (Child sel)

-- | Assign a stylesheet to a filter selector. When the selector is nested
-- inside an outer scope it will be composed with the `with` selector.

(&) :: Monad m => Refinement -> StyleT m () -> StyleT m ()
(&) p = nest (Self p)

-- | Root is used to add style rules to the top scope.

root :: Monad m => Selector -> StyleT m () -> StyleT m ()
root sel = nest (Root sel)

-- | Pop is used to add style rules to selectors defined in an outer scope. The
-- counter specifies how far up the scope stack we want to add the rules.

pop :: Monad m => Int -> StyleT m () -> StyleT m ()
pop i = nest (Pop i)

-------------------------------------------------------------------------------

nestQuery :: Monad m => Maybe NotOrOnly -> MediaType -> [Feature] -> StyleT m () -> StyleT m ()
nestQuery noo ty fs = modifyRules (pure . Query (MediaQuery noo ty fs))

-- | Apply a set of style rules when the media type and feature queries apply.

query :: Monad m => MediaType -> [Feature] -> StyleT m () -> StyleT m ()
query = nestQuery Nothing

-- | Apply a set of style rules when the media type and feature queries do not apply.

queryNot :: Monad m => MediaType -> [Feature] -> StyleT m () -> StyleT m ()
queryNot = nestQuery (Just Not)

-- | Apply a set of style rules only when the media type and feature queries apply.

queryOnly :: Monad m => MediaType -> [Feature] -> StyleT m () -> StyleT m ()
queryOnly = nestQuery (Just Only)

-------------------------------------------------------------------------------

keyframes :: Monad m => Text -> [(Double, StyleT m ())] -> StyleT m ()
keyframes n xs = rule . Keyframe . Keyframes n =<< traverse execSnd xs
  where
    execSnd (i, rs) = lift $ (i, ) <$> execStyleT rs

keyframesFromTo :: Monad m => Text -> StyleT m () -> StyleT m () -> StyleT m ()
keyframesFromTo n a b = keyframes n [(0, a), (100, b)]

-------------------------------------------------------------------------------

-- | Define a new font-face.

fontFace :: Monad m => StyleT m () -> StyleT m ()
fontFace = modifyRules (pure . Face)


-- | Import a CSS file from a URL

importUrl :: Style m => Text -> m ()
importUrl = rule . Import

-------------------------------------------------------------------------------

-- | Define a new custom selector.
-- Use 'customSel' to reference an already-defined selector.
--
-- > render $ do
-- >   sel <- defineCustomSel "example" (header |> nav)
-- >   body |> sel ** a ?
-- >     do background white
--
-- > @custom-selector :--example header > nav;
-- > body > :--example a {
-- >   background: white;
-- > }

defineCustomSel :: Style m => Text -> Selector -> m Refinement
defineCustomSel n s = do
  rule $ CustomSelector n s
  pure $ customSel n

-------------------------------------------------------------------------------

-- | Reference a custom media query by name.
-- Use 'defineCustomMedia' to define a custom media query.
--
-- > render $ ".test" ? do
-- >   media <- customMedia "example"
-- >   query media & background white
--
-- > @media(--example) {
-- >   .test {
-- >     background: white;
-- >   }
-- > }
customMedia :: Text -> Feature
customMedia = FeatureCustom

-- | Define a new custom mediaQuery.
-- Use 'customMedia' to reference an already-defined mediaQuery.
--
-- > render $ ".test" ? do
-- >   media <- defineCustomMedia "example" . CustomMediaQueryList . singleton $ minWidth (px 20)
-- >   query media & background white
--
-- > @custom-media --example (min-width: 20px);
-- > @media (--example) {
-- >   .test {
-- >     background: white;
-- >   }
-- > }

defineCustomMedia :: Text -> CustomMediaQuery -> StyleM Feature
defineCustomMedia n s = do
  rule $ CustomMedia n s
  pure $ customMedia n

-------------------------------------------------------------------------------

-- | Indicate the supplied css should override css declarations that would
-- otherwise take precedence.
--
-- Use sparingly.
important :: Monad m => StyleT m a -> StyleT m a
important = modifyRules (addImportant <$>)

-- The last case indicates there may be something wrong in the typing, as
-- it shouldn't be possible to make a non-property important. In practice,
-- this implementation means only the directly applied property rule is
-- affected, i.e. no nested rules. That could be changed by adding recursive cases.
addImportant :: Rule -> Rule
addImportant (Property ms@(filter (isJust . _Important) -> (_:_)) k v) =
  Property ms k v
addImportant (Property ms k v  ) = Property (Important : ms) k v
addImportant (PropertyAxial ms@(filter (isJust . _Important) -> (_:_)) k v) =
  PropertyAxial ms k v
addImportant (PropertyAxial ms k v  ) = PropertyAxial (Important : ms) k v
addImportant (PropertyDirectional ms@(filter (isJust . _Important) -> (_:_)) k v) =
  PropertyDirectional ms k v
addImportant (PropertyDirectional ms k v  ) = PropertyDirectional (Important : ms) k v
addImportant r                   = r

-------------------------------------------------------------------------------

-- | Set the value of all properties except for unicode-bidi, direction, and custom properties.
-- | > all_ unset
-- | > all_ initial
-- | > all_ revert
all_ :: Monad m => Value -> StyleT m ()
all_ = key "all"

-------------------------------------------------------------------------------

newtype CustomProp = CustomProp Text
  deriving (Show, Eq, IsString)

instance Val CustomProp where
  value k = Value $ Plain $ mconcat ["var(", customPropToText k, ")"]

customPropToText :: CustomProp -> Text
customPropToText (CustomProp k) = "--" <> k

-- | Set a custom prop.
-- | > primary = customProp @Color "primary"
-- | > do
-- | >   primary red
-- | >   color (var "primary")
-- |
-- | > --primary: #ff0000;
-- | > color: var(--primary);
customProp :: (Val a, Style m) => CustomProp -> a -> m ()
customProp k = key $ Key $ Plain $ customPropToText k

-- | Read a custom prop.
-- | Note that the value cascades down the DOM tree.
var :: Other a => CustomProp -> a
var = other . value

-- | Define a getter and a setter for a custom property.
-- | Note that the value of the getter cascades down the DOM tree.
-- | > (primary, setPrimary) = defineCustomProp @Color "primary"
-- | > do
-- | >   setPrimary red
-- | >   color primary
-- |
-- | > --primary: #ff0000;
-- | > color: var(--primary);
defineCustomProp :: (Val a, Other a, Style m) => CustomProp -> (a, a -> m ())
defineCustomProp k = (var k, customProp k)
