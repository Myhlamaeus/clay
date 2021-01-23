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

import Clay.Border.V2
import Clay.BoxModel (Stroke, dashed, dotted, double, groove, inset, mkBorder, outset, ridge, solid, wavy)
import qualified Clay.BoxModel as BM
import Clay.Color
import Clay.Directional
import Clay.Size
import Clay.Stylesheet

border, borderTop, borderLeft, borderBottom, borderRight :: Style m => Size LengthUnit -> Stroke -> Color -> m ()

border        w s c = BM.border $ all' $ mkBorder w s c
borderTop     w s c = BM.border $ blockStart $ mkBorder w s c
borderLeft    w s c = BM.border $ inlineStart $ mkBorder w s c
borderBottom  w s c = BM.border $ blockEnd $ mkBorder w s c
borderRight   w s c = BM.border $ inlineEnd $ mkBorder w s c

borderColor4 :: Style m => Color -> Color -> Color -> Color -> m ()
borderColor4 bs ie be is = BM.borderColor $ eachDir bs is be ie

borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor :: Style m => Color -> m ()

borderColor       = BM.borderColor . all'
borderTopColor    = BM.borderColor . blockStart
borderLeftColor   = BM.borderColor . inlineStart
borderBottomColor = BM.borderColor . blockEnd
borderRightColor  = BM.borderColor . inlineEnd

borderStyle4 :: Style m => Stroke -> Stroke -> Stroke -> Stroke -> m ()
borderStyle4 bs ie be is = BM.borderStyle $ eachDir bs is be ie

borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle :: Style m => Stroke -> m ()

borderStyle       = BM.borderStyle . all'
borderTopStyle    = BM.borderStyle . blockStart
borderLeftStyle   = BM.borderStyle . inlineStart
borderBottomStyle = BM.borderStyle . blockEnd
borderRightStyle  = BM.borderStyle . inlineEnd

borderWidth4 :: Style m => Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> m ()
borderWidth4 bs ie be is = BM.borderWidth $ eachDir bs is be ie

borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Style m => Size LengthUnit -> m ()

borderWidth       = BM.borderWidth . all'
borderTopWidth    = BM.borderWidth . blockStart
borderLeftWidth   = BM.borderWidth . inlineStart
borderBottomWidth = BM.borderWidth . blockEnd
borderRightWidth  = BM.borderWidth . inlineEnd

-------------------------------------------------------------------------------

outline, outlineTop, outlineLeft, outlineBottom, outlineRight :: Style m => Stroke -> Size LengthUnit -> Color -> m ()

outline        s w c = BM.outline $ all' $ mkBorder w s c
outlineTop     s w c = BM.outline $ blockStart $ mkBorder w s c
outlineLeft    s w c = BM.outline $ inlineStart $ mkBorder w s c
outlineBottom  s w c = BM.outline $ blockEnd $ mkBorder w s c
outlineRight   s w c = BM.outline $ inlineEnd $ mkBorder w s c

outlineColor4 :: Style m => Color -> Color -> Color -> Color -> m ()
outlineColor4 bs ie be is = BM.outlineColor $ eachDir bs is be ie

outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor :: Style m => Color -> m ()

outlineColor       = BM.outlineColor . all'
outlineTopColor    = BM.outlineColor . blockStart
outlineLeftColor   = BM.outlineColor . inlineStart
outlineBottomColor = BM.outlineColor . blockEnd
outlineRightColor  = BM.outlineColor . inlineEnd

outlineStyle4 :: Style m => Stroke -> Stroke -> Stroke -> Stroke -> m ()
outlineStyle4 bs ie be is = BM.outlineStyle $ eachDir bs is be ie

outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle :: Style m => Stroke -> m ()

outlineStyle       = BM.outlineStyle . all'
outlineTopStyle    = BM.outlineStyle . blockStart
outlineLeftStyle   = BM.outlineStyle . inlineStart
outlineBottomStyle = BM.outlineStyle . blockEnd
outlineRightStyle  = BM.outlineStyle . inlineEnd

outlineWidth4 :: Style m => Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> Size LengthUnit -> m ()
outlineWidth4 bs ie be is = BM.outlineWidth $ eachDir bs is be ie

outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth :: Style m => Size LengthUnit -> m ()

outlineWidth       = BM.outlineWidth . all'
outlineTopWidth    = BM.outlineWidth . blockStart
outlineLeftWidth   = BM.outlineWidth . inlineStart
outlineBottomWidth = BM.outlineWidth . blockEnd
outlineRightWidth  = BM.outlineWidth . inlineEnd

outlineOffset :: Style m => Size LengthUnit -> m ()
outlineOffset = key "outline-offset"
