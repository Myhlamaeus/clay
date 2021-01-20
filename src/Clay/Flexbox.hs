{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Clay.Flexbox where

import Clay.Common     (Auto, Baseline, Center, GlobalValues, Other)
import Clay.Property
import Clay.Size       (Size)
import Clay.Stylesheet
import Data.String     (fromString)

-- | CSS Flexible Box Layout
-- http://dev.w3.org/csswg/css-flexbox-1

class FlexEnd      a where flexEnd      :: a
class FlexStart    a where flexStart    :: a
class SpaceAround  a where spaceAround  :: a
class SpaceBetween a where spaceBetween :: a
class Stretch      a where stretch      :: a

instance FlexEnd Value      where flexEnd      = "flex-end"
instance FlexStart Value    where flexStart    = "flex-start"
instance SpaceAround Value  where spaceAround  = "space-around"
instance SpaceBetween Value where spaceBetween = "space-between"
instance Stretch Value      where stretch      = "stretch"

-------------------------------------------------------------------------------

newtype AlignContentValue = AlignContentValue Value
  deriving (Val, Other, GlobalValues, FlexStart, FlexEnd
          , Center, SpaceBetween, SpaceAround, Stretch)

alignContent :: Style m => AlignContentValue -> m ()
alignContent = key "align-content"

-------------------------------------------------------------------------------

newtype AlignItemsValue = AlignItemValue Value
  deriving (Val, Other, GlobalValues, Baseline
          , Center, FlexEnd, FlexStart, Stretch)

alignItems :: Style m => AlignItemsValue -> m ()
alignItems = key "align-items"

-------------------------------------------------------------------------------

newtype AlignSelfValue = AlignSelfValue Value
  deriving (Val, Other, GlobalValues, Auto, Baseline
          , Center, FlexEnd, FlexStart, Stretch)

alignSelf :: Style m => AlignSelfValue -> m ()
alignSelf = key "align-self"

-------------------------------------------------------------------------------

flex :: Style m => Int -> Int -> Size b -> m ()
flex g s b = key "flex" (gs ! ss ! value b)
  where gs = fromString (show g) :: Value
        ss = fromString (show s) :: Value

-------------------------------------------------------------------------------

flexBasis :: Style m => Size a -> m ()
flexBasis = key "flex-basis"

-------------------------------------------------------------------------------

newtype FlexDirection = FlexDirection Value
  deriving (Val, Other)

row, rowReverse, column, columnReverse :: FlexDirection

row           = FlexDirection "row"
rowReverse    = FlexDirection "row-reverse"
column        = FlexDirection "column"
columnReverse = FlexDirection "column-reverse"

flexDirection :: Style m => FlexDirection -> m ()
flexDirection = key "flex-direction"

-------------------------------------------------------------------------------

flexFlow :: Style m => FlexDirection -> FlexWrap -> m ()
flexFlow d w = key "flex-flow" (d ! w)

-------------------------------------------------------------------------------

flexGrow :: Style m => Int -> m ()
flexGrow i = key "flex-grow" (fromString (show i) :: Value)

flexShrink :: Style m => Int  -> m ()
flexShrink i = key "flex-shrink" (fromString (show i) :: Value)

-------------------------------------------------------------------------------

newtype FlexWrap = FlexWrap Value
  deriving (Val, Other)

nowrap, wrap, wrapReverse :: FlexWrap

nowrap = FlexWrap "nowrap"
wrap = FlexWrap "wrap"
wrapReverse = FlexWrap "wrap-reverse"

flexWrap :: Style m => FlexWrap -> m ()
flexWrap = key "flex-wrap"

-------------------------------------------------------------------------------

newtype JustifyContentValue = JustifyContentValue Value
  deriving (Val, Other, GlobalValues, Center, FlexEnd
          , FlexStart, SpaceAround, SpaceBetween)

justifyContent :: Style m => JustifyContentValue -> m ()
justifyContent = key "justify-content"

-------------------------------------------------------------------------------

order :: Style m => Int -> m ()
order i = key "order" (fromString (show i) :: Value)
