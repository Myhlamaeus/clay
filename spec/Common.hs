{-# LANGUAGE RankNTypes #-}
module Common where

import Test.Hspec
import Clay
import Clay.Render
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text as T

shouldRenderFromWith :: Config -> Text -> Css -> SpecWith ()
shouldRenderFromWith cfg txt css =
  it ("renders " <> unpack txt) $ shouldRenderItFromWith cfg txt css

shouldRenderAsFromWith :: Config -> String -> Text -> Css -> SpecWith ()
shouldRenderAsFromWith cfg des txt css =
  it ("renders " <> des) $ shouldRenderItFromWith cfg txt css

shouldRenderItFromWith :: Config -> Text -> Css -> Expectation
shouldRenderItFromWith cfg = flip $ shouldBe . renderWith cfg []

shouldRenderFrom :: Text -> Css -> SpecWith ()
shouldRenderFrom = shouldRenderFromWith compact
infixr 0 `shouldRenderFrom`

shouldRenderAsFrom :: String -> Text -> Css -> SpecWith ()
shouldRenderAsFrom = shouldRenderAsFromWith compact
infixr 3 `shouldRenderAsFrom`

shouldRenderItFrom :: Text -> Css -> Expectation
shouldRenderItFrom = shouldRenderItFromWith compact
infixr 0 `shouldRenderItFrom`

shouldRenderLogicalFrom :: Text -> Css -> SpecWith ()
shouldRenderLogicalFrom = shouldRenderFromWith (useLogical compact)
infixr 0 `shouldRenderLogicalFrom`

shouldRenderLogicalAsFrom :: String -> Text -> Css -> SpecWith ()
shouldRenderLogicalAsFrom = shouldRenderAsFromWith (useLogical compact)
infixr 3 `shouldRenderLogicalAsFrom`

shouldRenderLogicalItFrom :: Text -> Css -> Expectation
shouldRenderLogicalItFrom = shouldRenderItFromWith (useLogical compact)
infixr 0 `shouldRenderLogicalItFrom`

describe' :: Val a => String -> [a] -> SpecWith b -> SpecWith b
describe' name vals = describe $ ((name <> " ") <>) $ (<> ")") $ ("(" <>) $ intercalate ", " $ (T.unpack . plain . unValue . value <$> vals)

testAxialDefault :: Val a => (Axial a -> Css) -> a -> a -> Text -> Text -> Text -> Text -> SpecWith ()
testAxialDefault prop b i all'' eachAxis'' block'' inline'' = describe "default" $ do
  describe' "all'" [b] $ shouldRenderAsFrom (unpack all'') all'' $ prop $ all' b
  describe' "eachAxis" [b, i] $ shouldRenderAsFrom (unpack eachAxis'') eachAxis'' $ prop $ eachAxis b i
  describe' "block'" [b] $ shouldRenderAsFrom (unpack block'') block'' $ prop $ block' b
  describe' "inline'" [i] $ shouldRenderAsFrom (unpack inline'') inline'' $ prop $ inline' i

testAxialLogical :: Val a => (Axial a -> Css) -> a -> a -> Text -> Text -> Text -> Text -> SpecWith ()
testAxialLogical prop b i all'' eachAxis'' block'' inline'' = describe "logical" $ do
  describe' "all'" [b] $ shouldRenderLogicalAsFrom (unpack all'') all'' $ prop $ all' b
  describe' "eachAxis" [b, i] $ shouldRenderLogicalAsFrom (unpack eachAxis'') eachAxis'' $ prop $ eachAxis b i
  describe' "block'" [b] $ shouldRenderLogicalAsFrom (unpack block'') block'' $ prop $ block' b
  describe' "inline'" [i] $ shouldRenderLogicalAsFrom (unpack inline'') inline'' $ prop $ inline' i

testDirectionalDefault :: Val a => (forall dir. IsDirectional dir => dir a -> Css) -> a -> a -> a -> a -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> SpecWith ()
testDirectionalDefault prop bs is be ie all'' eachAxis'' block'' inline'' eachDir'' start'' end'' blockStart'' inlineStart'' blockEnd'' inlineEnd'' = describe "default" $ do
  describe' "all'" [bs] $ shouldRenderAsFrom (unpack all'') all'' $ prop $ all' bs
  describe' "eachAxis" [bs, is] $ shouldRenderAsFrom (unpack eachAxis'') eachAxis'' $ prop $ eachAxis bs is
  describe' "block'" [bs] $ shouldRenderAsFrom (unpack block'') block'' $ prop $ block' bs
  describe' "inline'" [is] $ shouldRenderAsFrom (unpack inline'') inline'' $ prop $ inline' is
  describe' "eachDir" [bs, is, be, ie] $ shouldRenderAsFrom (unpack eachDir'') eachDir'' $ prop $ eachDir bs is be ie
  describe' "start' . all'" [bs] $ shouldRenderAsFrom (unpack start'') start'' $ prop $ start' $ all' bs
  describe' "end' . all'" [be] $ shouldRenderAsFrom (unpack end'') end'' $ prop $ end' $ all' be
  describe' "blockStart" [bs] $ shouldRenderAsFrom (unpack blockStart'') blockStart'' $ prop $ blockStart bs
  describe' "inlineStart" [is] $ shouldRenderAsFrom (unpack inlineStart'') inlineStart'' $ prop $ inlineStart is
  describe' "blockEnd" [be] $ shouldRenderAsFrom (unpack blockEnd'') blockEnd'' $ prop $ blockEnd be
  describe' "inlineEnd" [ie] $ shouldRenderAsFrom (unpack inlineEnd'') inlineEnd'' $ prop $ inlineEnd ie

testDirectionalLogical :: Val a => (forall dir. IsDirectional dir => dir a -> Css) -> a -> a -> a -> a -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> SpecWith ()
testDirectionalLogical prop bs is be ie all'' eachAxis'' block'' inline'' eachDir'' start'' end'' blockStart'' inlineStart'' blockEnd'' inlineEnd'' = describe "logical" $ do
  describe' "all'" [bs] $ shouldRenderLogicalAsFrom (unpack all'') all'' $ prop $ all' bs
  describe' "eachAxis" [bs, is] $ shouldRenderLogicalAsFrom (unpack eachAxis'') eachAxis'' $ prop $ eachAxis bs is
  describe' "block'" [bs] $ shouldRenderLogicalAsFrom (unpack block'') block'' $ prop $ block' bs
  describe' "inline'" [is] $ shouldRenderLogicalAsFrom (unpack inline'') inline'' $ prop $ inline' is
  describe' "eachDir" [bs, is, be, ie] $ shouldRenderLogicalAsFrom (unpack eachDir'') eachDir'' $ prop $ eachDir bs is be ie
  describe' "start' . all'" [bs] $ shouldRenderLogicalAsFrom (unpack start'') start'' $ prop $ start' $ all' bs
  describe' "end' . all'" [be] $ shouldRenderLogicalAsFrom (unpack end'') end'' $ prop $ end' $ all' be
  describe' "blockStart" [bs] $ shouldRenderLogicalAsFrom (unpack blockStart'') blockStart'' $ prop $ blockStart bs
  describe' "inlineStart" [is] $ shouldRenderLogicalAsFrom (unpack inlineStart'') inlineStart'' $ prop $ inlineStart is
  describe' "blockEnd" [be] $ shouldRenderLogicalAsFrom (unpack blockEnd'') blockEnd'' $ prop $ blockEnd be
  describe' "inlineEnd" [ie] $ shouldRenderLogicalAsFrom (unpack inlineEnd'') inlineEnd'' $ prop $ inlineEnd ie

testCornerDirectionalDefault :: Val a => (CornerDirectional a -> Css) -> a -> a -> a -> a -> Text -> Text -> Text -> Text -> Text -> Text -> SpecWith ()
testCornerDirectionalDefault prop ss es ee se allCorners'' eachCorner'' startStart'' endStart'' endEnd'' startEnd'' = describe "default" $ do
  describe' "allCorners" [ss] $ shouldRenderAsFrom (unpack allCorners'') allCorners'' $ prop $ allCorners ss
  describe' "eachCorner" [ss, es, ee, se] $ shouldRenderAsFrom (unpack eachCorner'') eachCorner'' $ prop $ eachCorner ss es ee se
  describe' "startStart" [ss] $ shouldRenderAsFrom (unpack startStart'') startStart'' $ prop $ startStart ss
  describe' "endStart" [es] $ shouldRenderAsFrom (unpack endStart'') endStart'' $ prop $ endStart es
  describe' "endEnd" [ee] $ shouldRenderAsFrom (unpack endEnd'') endEnd'' $ prop $ endEnd ee
  describe' "startEnd" [se] $ shouldRenderAsFrom (unpack startEnd'') startEnd'' $ prop $ startEnd se

testCornerDirectionalLogical :: Val a => (CornerDirectional a -> Css) -> a -> a -> a -> a -> Text -> Text -> Text -> Text -> Text -> Text -> SpecWith ()
testCornerDirectionalLogical prop ss es ee se allCorners'' eachCorner'' startStart'' endStart'' endEnd'' startEnd'' = describe "logical" $ do
  describe' "allCorners" [ss] $ shouldRenderLogicalAsFrom (unpack allCorners'') allCorners'' $ prop $ allCorners ss
  describe' "eachCorner" [ss, es, ee, se] $ shouldRenderLogicalAsFrom (unpack eachCorner'') eachCorner'' $ prop $ eachCorner ss es ee se
  describe' "startStart" [ss] $ shouldRenderLogicalAsFrom (unpack startStart'') startStart'' $ prop $ startStart ss
  describe' "endStart" [es] $ shouldRenderLogicalAsFrom (unpack endStart'') endStart'' $ prop $ endStart es
  describe' "endEnd" [ee] $ shouldRenderLogicalAsFrom (unpack endEnd'') endEnd'' $ prop $ endEnd ee
  describe' "startEnd" [se] $ shouldRenderLogicalAsFrom (unpack startEnd'') startEnd'' $ prop $ startEnd se
