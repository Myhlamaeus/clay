{-# LANGUAGE OverloadedStrings #-}
module Clay.Border.V2
(
-- * Collapsing borders model for a table
  borderCollapse
, borderSpacing, borderSpacing2
)
where

import Clay.Property
import Clay.Stylesheet
import Clay.Size
import Clay.Display

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
