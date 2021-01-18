{-# LANGUAGE OverloadedStrings #-}
module Clay.Border
(
-- * Stroke type, used for border-style and outline-style.
  Stroke
, solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

-- * Border properties.

, border, borderTop, borderLeft, borderBottom, borderRight
, borderColor4, borderColor, borderTopColor, borderLeftColor, borderBottomColor, borderRightColor
, borderStyle4, borderStyle, borderTopStyle, borderLeftStyle, borderBottomStyle, borderRightStyle
, borderWidth4, borderWidth, borderTopWidth, borderLeftWidth, borderBottomWidth, borderRightWidth

-- * Outline properties.

, outline, outlineTop, outlineLeft, outlineBottom, outlineRight
, outlineColor4, outlineColor, outlineTopColor, outlineLeftColor, outlineBottomColor, outlineRightColor
, outlineStyle4, outlineStyle, outlineTopStyle, outlineLeftStyle, outlineBottomStyle, outlineRightStyle
, outlineWidth4, outlineWidth, outlineTopWidth, outlineLeftWidth, outlineBottomWidth, outlineRightWidth
, outlineOffset

-- * Border radius.

, borderRadius
, borderTopLeftRadius, borderTopRightRadius
, borderBottomLeftRadius, borderBottomRightRadius

-- * Collapsing borders model for a table
, borderCollapse
, borderSpacing, borderSpacing2
)
where

import           Clay.Directional
import Clay.Property
import Clay.Stylesheet
import Clay.Color
import Clay.Size
import Clay.Display
import qualified Clay.BoxModel as BM
import Clay.BoxModel (Stroke, solid, dotted, dashed, double, wavy, groove, ridge, inset, outset, mkBorder)

border, borderTop, borderLeft, borderBottom, borderRight :: Size LengthUnit -> Stroke -> Color -> Css

border        w s c = BM.border $ all' $ mkBorder w s c
borderTop     w s c = BM.border $ blockStart $ mkBorder w s c
borderLeft    w s c = BM.border $ inlineStart $ mkBorder w s c
borderBottom  w s c = BM.border $ blockEnd $ mkBorder w s c
borderRight   w s c = BM.border $ inlineEnd $ mkBorder w s c

borderColor4 :: Color -> Color -> Color -> Color -> Css
borderColor4 bs ie be is = BM.borderColor $ eachDir bs is be ie

borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor :: Color -> Css

borderColor       = BM.borderColor . all'
borderTopColor    = BM.borderColor . blockStart
borderLeftColor   = BM.borderColor . inlineStart
borderBottomColor = BM.borderColor . blockEnd
borderRightColor  = BM.borderColor . inlineEnd

borderStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Css
borderStyle4 bs ie be is = BM.borderStyle $ eachDir bs is be ie

borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle :: Stroke -> Css

borderStyle       = BM.borderStyle . all'
borderTopStyle    = BM.borderStyle . blockStart
borderLeftStyle   = BM.borderStyle . inlineStart
borderBottomStyle = BM.borderStyle . blockEnd
borderRightStyle  = BM.borderStyle . inlineEnd

borderWidth4 :: Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Css
borderWidth4 bs ie be is = BM.borderWidth $ eachDir bs is be ie

borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size LengthUnit -> Css

borderWidth       = BM.borderWidth . all'
borderTopWidth    = BM.borderWidth . blockStart
borderLeftWidth   = BM.borderWidth . inlineStart
borderBottomWidth = BM.borderWidth . blockEnd
borderRightWidth  = BM.borderWidth . inlineEnd

-------------------------------------------------------------------------------

outline, outlineTop, outlineLeft, outlineBottom, outlineRight :: Stroke -> Size LengthUnit -> Color -> Css

outline        s w c = BM.outline $ all' $ mkBorder w s c
outlineTop     s w c = BM.outline $ blockStart $ mkBorder w s c
outlineLeft    s w c = BM.outline $ inlineStart $ mkBorder w s c
outlineBottom  s w c = BM.outline $ blockEnd $ mkBorder w s c
outlineRight   s w c = BM.outline $ inlineEnd $ mkBorder w s c

outlineColor4 :: Color -> Color -> Color -> Color -> Css
outlineColor4 bs ie be is = BM.outlineColor $ eachDir bs is be ie

outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor :: Color -> Css

outlineColor       = BM.outlineColor . all'
outlineTopColor    = BM.outlineColor . blockStart
outlineLeftColor   = BM.outlineColor . inlineStart
outlineBottomColor = BM.outlineColor . blockEnd
outlineRightColor  = BM.outlineColor . inlineEnd

outlineStyle4 :: Stroke -> Stroke -> Stroke -> Stroke -> Css
outlineStyle4 bs ie be is = BM.outlineStyle $ eachDir bs is be ie

outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle :: Stroke -> Css

outlineStyle       = BM.outlineStyle . all'
outlineTopStyle    = BM.outlineStyle . blockStart
outlineLeftStyle   = BM.outlineStyle . inlineStart
outlineBottomStyle = BM.outlineStyle . blockEnd
outlineRightStyle  = BM.outlineStyle . inlineEnd

outlineWidth4 :: Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Css
outlineWidth4 bs ie be is = BM.outlineWidth $ eachDir bs is be ie

outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth :: Size LengthUnit -> Css

outlineWidth       = BM.outlineWidth . all'
outlineTopWidth    = BM.outlineWidth . blockStart
outlineLeftWidth   = BM.outlineWidth . inlineStart
outlineBottomWidth = BM.outlineWidth . blockEnd
outlineRightWidth  = BM.outlineWidth . inlineEnd

outlineOffset :: Size LengthUnit -> Css
outlineOffset = key "outline-offset"

-------------------------------------------------------------------------------

-- @TODO borderRadius is using a Directional rotated by 1/8; should we use the same structure?

borderRadius :: Size a -> Size a -> Size a -> Size a -> Css
borderRadius a b c d = key "border-radius" (a ! b ! c ! d)

borderTopLeftRadius, borderTopRightRadius,
  borderBottomLeftRadius, borderBottomRightRadius :: Size a -> Size a -> Css

borderTopLeftRadius     a b = key "border-top-left-radius"     (a ! b)
borderTopRightRadius    a b = key "border-top-right-radius"    (a ! b)
borderBottomLeftRadius  a b = key "border-bottom-left-radius"  (a ! b)
borderBottomRightRadius a b = key "border-bottom-right-radius" (a ! b)

-------------------------------------------------------------------------------

{- newtype Collapse = Collapse Value
  deriving (Val, GlobalValues, Other)

collapseCollapse, collapseSeparate :: Collapse

collapseCollapse = Collapse "collapse"
collapseSeparate  = Collapse "separate" -}

{-  Due conflict with Visibility collapse
    Preferred just to add separate to Visibility
    Because (borderCollapse collapseCollapse) sounds bad -}

borderCollapse :: Visibility -> Css
borderCollapse = key "border-collapse"

borderSpacing :: Size a -> Css
borderSpacing = key "border-spacing"

borderSpacing2 :: Size a -> Size a -> Css
borderSpacing2 a b = key "border-spacing" (a ! b)
