{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clay.BoxModel
(
-- * Sizing.
  boxSize, minBoxSize, maxBoxSize

-- * Padding.
, padding

-- * Scroll padding.
, scrollPadding

-- * Margin.
, margin

-- * Scroll margin.
, scrollMargin

-- * Positioning.
, posInset

-- * Stroke type, used for border-style and outline-style.
, Stroke
, solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

-- * Border type, the width, style and color of both border and outline.
, Border
, mkBorder

-- * Border properties.
, border, borderWidth, borderStyle, borderColor, borderRadius

-- * Outline properties.
, outline, outlineWidth, outlineStyle, outlineColor
)
where

import           Clay.Common
import           Clay.Color
import           Clay.Directional
import           Clay.Property
import           Clay.Size
import           Clay.Stylesheet
import Data.These

boxSize, minBoxSize, maxBoxSize :: Style m => Axial (Size a) -> m ()

-- | Set the size of a box
-- >>> render $ boxSize $ block' $ px 5
-- height: 5px;
--
-- >>> renderLogical $ boxSize $ block' $ px 5
-- block-size: 5px;
boxSize = keyAxial (PartedKey [] (That "size"))

-- | Set the size of a box
-- >>> render $ minBoxSize $ inline' $ px 5
-- min-width: 5px;
--
-- >>> renderLogical $ minBoxSize $ inline' $ px 5
-- min-inline-size: 5px;
minBoxSize = keyAxial (PartedKey [] (These "min" "size"))

-- | Set the size of a box
-- >>> render $ maxBoxSize $ all' $ px 5
-- max-height: 5px;
-- max-width: 5px;
--
-- >>> renderLogical $ maxBoxSize $ all' $ px 5
-- max-block-size: 5px;
-- max-inline-size: 5px;
maxBoxSize = keyAxial (PartedKey [] (These "max" "size"))

-------------------------------------------------------------------------------

-- | Set the padding of a box
-- >>> render $ padding $ all' $ px 5
-- padding: 5px;
--
-- >>> renderLogical $ padding $ all' $ px 5
-- padding: 5px;
padding :: (IsDirectional dir, Style m) => dir (Size a) -> m ()
padding = keyDirectional "padding" . toDirectional

-------------------------------------------------------------------------------

-- | Set the scroll-padding of a box
-- >>> render $ scrollPadding $ block' $ px 5
-- scroll-padding-block: 5px;
--
-- >>> renderLogical $ scrollPadding $ block' $ px 5
-- scroll-padding-block: 5px;
scrollPadding :: (IsDirectional dir, Style m) => dir (Size a) -> m ()
scrollPadding = keyDirectional "scroll-padding" . toDirectional

-------------------------------------------------------------------------------

-- | Set the margin of a box
-- >>> render $ margin $ eachAxis (px 5) (px 10)
-- margin: 5px 10px;
--
-- >>> renderLogical $ margin $ eachAxis (px 5) (px 10)
-- margin: logical 5px 10px;
margin :: (IsDirectional dir, Style m) => dir (Size a) -> m ()
margin = keyDirectional "margin" . toDirectional

-------------------------------------------------------------------------------

-- | Set the scroll-margin of a box
-- >>> render $ margin $ eachDir (px 5) (px 10) (px 15) (px 20)
-- scroll-margin: 5px 20px 15px 10px;
--
-- >>> renderLogical $ margin $ eachDir (px 5) (px 10) (px 15) (px 20)
-- scroll-margin: logical 5px 10px 15px 20px;
scrollMargin :: (IsDirectional dir, Style m) => dir (Size a) -> m ()
scrollMargin = keyDirectional "scroll-margin" . toDirectional

-------------------------------------------------------------------------------

-- | Set the inset of a box
-- >>> render $ posInset $ start' $ px 5
-- top: 5px;
-- left: 5px;
--
-- >>> renderLogical $ margin $ start' $ px 5
-- inset-block-start: 5px;
-- inset-inline-start: 5px;
posInset :: (IsDirectional dir, Style m) => dir (Size a) -> m ()
posInset = keyDirectional "inset" . toDirectional

-------------------------------------------------------------------------------

newtype Stroke = Stroke Value
  deriving (Show, Eq, Val, Other, GlobalValues, Auto, None)

solid, dotted, dashed, double, wavy, groove, ridge, inset, outset :: Stroke

solid  = Stroke "solid"
dotted = Stroke "dotted"
dashed = Stroke "dashed"
double = Stroke "double"
wavy   = Stroke "wavy"
groove = Stroke "groove"
ridge  = Stroke "ridge"
inset  = Stroke "inset"
outset = Stroke "outset"

-------------------------------------------------------------------------------

data Border a
  = Border (Size a) Stroke Color
  | BorderOther Value
  deriving (Show)
  deriving (GlobalValues) via (GlobalValuesViaOther (Border a))

instance Val (Border a) where
  value (Border w s c) = value (w ! s ! c)
  value (BorderOther v) = v

instance Other (Border a) where other = BorderOther
instance None (Border a) where none = other none

-- | Specify that border width, style, and color should be set.
-- >>> render $ border $ blockStart' $ mkBorder (px 5) dashed yellow
-- border-top: 5px dashed rgb(255, 255, 0);
--
-- >>> renderLogical $ border $ blockStart' $ mkBorder (px 5) dashed yellow
-- border-block-start: 5px dashed rgb(255, 255, 0);
mkBorder :: Size a -> Stroke -> Color -> Border a
mkBorder = Border

-------------------------------------------------------------------------------

-- | Set the border of a box
-- >>> render $ border $ inlineEnd' $ mkBorder (px 5) dashed yellow
-- border-right: 5px dashed rgb(255, 255, 0);
--
-- >>> renderLogical $ border $ inlineEnd' $ mkBorder (px 5) solid yellow
-- border-inline-end: 5px solid rgb(255, 255, 0);
border :: (IsDirectional dir, Style m) => dir (Border LengthUnit) -> m ()
border = keyDirectional "border" . toDirectional

-- | Set the border-width of a box
-- >>> render $ borderWidth $ inlineEnd' $ px 5
-- border-right-width: 5px;
--
-- >>> renderLogical $ borderWidth $ inlineEnd' $ px 5
-- border-inline-end-width: 5px;
borderWidth :: (IsDirectional dir, Style m) => dir (Size LengthUnit) -> m ()
borderWidth = keyDirectional (PartedKey [] (These "border" "width")) . toDirectional

-- | Set the border-style of a box
-- >>> render $ borderStyle $ inlineEnd' solid
-- border-right-style: solid;
--
-- >>> renderLogical $ borderStyle $ inlineEnd' solid
-- border-inline-end-style: solid;
borderStyle :: (IsDirectional dir, Style m) => dir Stroke -> m ()
borderStyle = keyDirectional (PartedKey [] (These "border" "style")) . toDirectional

-- | Set the border-color of a box
-- >>> render $ borderColor $ inlineEnd' yellow
-- border-right-color: rgb(255, 255, 0);
--
-- >>> renderLogical $ borderColor $ inlineEnd' yellow
-- border-inline-end-color: rgb(255, 255, 0);
borderColor :: (IsDirectional dir, Style m) => dir Color -> m ()
borderColor = keyDirectional (PartedKey [] (These "border" "color")) . toDirectional

-- | Set the border-radius of a box
-- >>> render $ borderRadius $ startStart $ px 5
-- border-top-left-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ startStart $ px 5
-- padding-start-start-radius: 5px;
borderRadius :: (Style m) => CornerDirectional (Size a, Size a) -> m ()
borderRadius = keyCornerDirectional (PartedKey [] (These "border" "radius"))

-------------------------------------------------------------------------------

-- | Set the outline of a box
-- >>> render $ outline $ inlineEnd' $ mkBorder (px 5) dashed yellow
-- outline-right: 5px dashed rgb(255, 255, 0);
--
-- >>> renderLogical $ outline $ inlineEnd' $ mkBorder (px 5) solid yellow
-- outline-inline-end: 5px solid rgb(255, 255, 0);
outline :: (IsDirectional dir, Style m) => dir (Border LengthUnit) -> m ()
outline = keyDirectional "outline" . toDirectional

-- | Set the outline-width of a box
-- >>> render $ outlineWidth $ inlineEnd' $ px 5
-- outline-right-width: 5px;
--
-- >>> renderLogical $ outlineWidth $ inlineEnd' $ px 5
-- outline-inline-end-width: 5px;
outlineWidth :: (IsDirectional dir, Style m) => dir (Size LengthUnit) -> m ()
outlineWidth = keyDirectional (PartedKey [] (These "outline" "width")) . toDirectional

-- | Set the outline-style of a box
-- >>> render $ outlineStyle $ inlineEnd' solid
-- outline-right-style: solid;
--
-- >>> renderLogical $ outlineStyle $ inlineEnd' solid
-- outline-inline-end-style: solid;
outlineStyle :: (IsDirectional dir, Style m) => dir Stroke -> m ()
outlineStyle = keyDirectional (PartedKey [] (These "outline" "style")) . toDirectional

-- | Set the outline-color of a box
-- >>> render $ outlineColor $ inlineEnd' yellow
-- outline-right-color: rgb(255, 255, 0);
--
-- >>> renderLogical $ outlineColor $ inlineEnd' yellow
-- outline-inline-end-color: rgb(255, 255, 0);
outlineColor :: (IsDirectional dir, Style m) => dir Color -> m ()
outlineColor = keyDirectional (PartedKey [] (These "outline" "color")) . toDirectional
