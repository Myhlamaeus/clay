{-# LANGUAGE OverloadedStrings #-}
module Clay.Border.V2
(
-- * Border radius.

  borderRadius
, borderTopLeftRadius, borderTopRightRadius
, borderBottomLeftRadius, borderBottomRightRadius

-- * Collapsing borders model for a table
, borderCollapse
, borderSpacing, borderSpacing2
)
where

import Clay.Property
import Clay.Stylesheet
import Clay.Size
import Clay.Display

-- @TODO borderRadius is using a Directional rotated by 1/8; should we use the same structure?

borderRadius :: Style m => Size a -> Size a -> Size a -> Size a -> m ()
borderRadius a b c d = key "border-radius" (a ! b ! c ! d)

borderTopLeftRadius, borderTopRightRadius,
  borderBottomLeftRadius, borderBottomRightRadius :: Style m => Size a -> Size a -> m ()

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

borderCollapse :: Style m => Visibility -> m ()
borderCollapse = key "border-collapse"

borderSpacing :: Style m => Size a -> m ()
borderSpacing = key "border-spacing"

borderSpacing2 :: Style m => Size a -> Size a -> m ()
borderSpacing2 a b = key "border-spacing" (a ! b)
