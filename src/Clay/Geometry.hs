{-# LANGUAGE OverloadedStrings #-}
module Clay.Geometry
(
-- * Sizing.
  width, height, minWidth, minHeight, maxWidth, maxHeight

-- * Padding.
, padding, paddingTop, paddingLeft, paddingRight, paddingBottom

-- * Margin.
, margin, marginTop, marginLeft, marginRight, marginBottom

-- * Positioning.
, size, top, left, bottom, right
)
where

import           Clay.Directional
import Clay.Stylesheet
import Clay.Size
import qualified Clay.BoxModel as BM
-- import Clay.BoxModel (Stroke, solid, dotted, dashed, double, wavy, groove, ridge, inset, outset, mkBorder)

width, height, minWidth, minHeight, maxWidth, maxHeight :: Size a -> Css

height    = BM.boxSize . block'
width     = BM.boxSize . inline'
minHeight = BM.minBoxSize . block'
minWidth  = BM.minBoxSize . inline'
maxHeight = BM.maxBoxSize . block'
maxWidth  = BM.maxBoxSize . inline'

-------------------------------------------------------------------------------

padding :: Size a -> Size a -> Size a -> Size a -> Css
padding bs ie be is = BM.padding $ eachDir bs is be ie

paddingTop, paddingLeft, paddingBottom, paddingRight :: Size a -> Css

paddingTop    = BM.padding . blockStart
paddingLeft   = BM.padding . inlineStart
paddingBottom = BM.padding . blockEnd
paddingRight  = BM.padding . inlineEnd

-------------------------------------------------------------------------------

margin :: Size a -> Size a -> Size a -> Size a -> Css
margin bs ie be is = BM.margin $ eachDir bs is be ie

marginTop, marginLeft, marginBottom, marginRight :: Size a -> Css

marginTop    = BM.margin . blockStart
marginLeft   = BM.margin . inlineStart
marginBottom = BM.margin . blockEnd
marginRight  = BM.margin . inlineEnd

-------------------------------------------------------------------------------

size, top, left, bottom, right :: Size a -> Css

size      = key "size"
top       = BM.posInset . blockStart
left      = BM.posInset . inlineStart
bottom    = BM.posInset . blockEnd
right     = BM.posInset . inlineEnd
