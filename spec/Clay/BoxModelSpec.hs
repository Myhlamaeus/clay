{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Clay.BoxModelSpec where

import Test.Hspec
import Clay hiding (padding, margin, border)
import Common
import Clay.BoxModel

spec :: Spec
spec = do
  describe "boxSize" $ do
    testAxialDefault boxSize (px 1) (px 2)
      "{height:1px;width:1px}"
      "{height:1px;width:2px}"
      "{height:1px}"
      "{width:2px}"
    testAxialLogical boxSize (px 1) (px 2)
      "{block-size:1px;inline-size:1px}"
      "{block-size:1px;inline-size:2px}"
      "{block-size:1px}"
      "{inline-size:2px}"
  describe "minBoxSize" $ do
    testAxialDefault minBoxSize (px 1) (px 2)
      "{min-height:1px;min-width:1px}"
      "{min-height:1px;min-width:2px}"
      "{min-height:1px}"
      "{min-width:2px}"
    testAxialLogical minBoxSize (px 1) (px 2)
      "{min-block-size:1px;min-inline-size:1px}"
      "{min-block-size:1px;min-inline-size:2px}"
      "{min-block-size:1px}"
      "{min-inline-size:2px}"
  describe "maxBoxSize" $ do
    testAxialDefault maxBoxSize (px 1) (px 2)
      "{max-height:1px;max-width:1px}"
      "{max-height:1px;max-width:2px}"
      "{max-height:1px}"
      "{max-width:2px}"
    testAxialLogical maxBoxSize (px 1) (px 2)
      "{max-block-size:1px;max-inline-size:1px}"
      "{max-block-size:1px;max-inline-size:2px}"
      "{max-block-size:1px}"
      "{max-inline-size:2px}"
  describe "padding" $ do
    testDirectionalDefault padding (px 1) (px 2) (px 3) (px 4)
      "{padding:1px}"
      "{padding:1px 2px}"
      "{padding-top:1px;padding-bottom:1px}"
      "{padding-left:2px;padding-right:2px}"
      "{padding:1px 4px 3px 2px}"
      "{padding-top:1px;padding-left:1px}"
      "{padding-bottom:3px;padding-right:3px}"
      "{padding-top:1px}"
      "{padding-left:2px}"
      "{padding-bottom:3px}"
      "{padding-right:4px}"
    testDirectionalLogical padding (px 1) (px 2) (px 3) (px 4)
      "{padding:1px}"
      "{padding:logical 1px 2px}"
      "{padding-block:1px}"
      "{padding-inline:2px}"
      "{padding:logical 1px 2px 3px 4px}"
      "{padding-block-start:1px;padding-inline-start:1px}"
      "{padding-block-end:3px;padding-inline-end:3px}"
      "{padding-block-start:1px}"
      "{padding-inline-start:2px}"
      "{padding-block-end:3px}"
      "{padding-inline-end:4px}"
  describe "scrollPadding" $ do
    testDirectionalDefault scrollPadding (px 1) (px 2) (px 3) (px 4)
      "{scroll-padding:1px}"
      "{scroll-padding:1px 2px}"
      "{scroll-padding-top:1px;scroll-padding-bottom:1px}"
      "{scroll-padding-left:2px;scroll-padding-right:2px}"
      "{scroll-padding:1px 4px 3px 2px}"
      "{scroll-padding-top:1px;scroll-padding-left:1px}"
      "{scroll-padding-bottom:3px;scroll-padding-right:3px}"
      "{scroll-padding-top:1px}"
      "{scroll-padding-left:2px}"
      "{scroll-padding-bottom:3px}"
      "{scroll-padding-right:4px}"
    testDirectionalLogical scrollPadding (px 1) (px 2) (px 3) (px 4)
      "{scroll-padding:1px}"
      "{scroll-padding:logical 1px 2px}"
      "{scroll-padding-block:1px}"
      "{scroll-padding-inline:2px}"
      "{scroll-padding:logical 1px 2px 3px 4px}"
      "{scroll-padding-block-start:1px;scroll-padding-inline-start:1px}"
      "{scroll-padding-block-end:3px;scroll-padding-inline-end:3px}"
      "{scroll-padding-block-start:1px}"
      "{scroll-padding-inline-start:2px}"
      "{scroll-padding-block-end:3px}"
      "{scroll-padding-inline-end:4px}"
  describe "margin" $ do
    testDirectionalDefault margin (px 1) (px 2) (px 3) (px 4)
      "{margin:1px}"
      "{margin:1px 2px}"
      "{margin-top:1px;margin-bottom:1px}"
      "{margin-left:2px;margin-right:2px}"
      "{margin:1px 4px 3px 2px}"
      "{margin-top:1px;margin-left:1px}"
      "{margin-bottom:3px;margin-right:3px}"
      "{margin-top:1px}"
      "{margin-left:2px}"
      "{margin-bottom:3px}"
      "{margin-right:4px}"
    testDirectionalLogical margin (px 1) (px 2) (px 3) (px 4)
      "{margin:1px}"
      "{margin:logical 1px 2px}"
      "{margin-block:1px}"
      "{margin-inline:2px}"
      "{margin:logical 1px 2px 3px 4px}"
      "{margin-block-start:1px;margin-inline-start:1px}"
      "{margin-block-end:3px;margin-inline-end:3px}"
      "{margin-block-start:1px}"
      "{margin-inline-start:2px}"
      "{margin-block-end:3px}"
      "{margin-inline-end:4px}"
  describe "scrollMargin" $ do
    testDirectionalDefault scrollMargin (px 1) (px 2) (px 3) (px 4)
      "{scroll-margin:1px}"
      "{scroll-margin:1px 2px}"
      "{scroll-margin-top:1px;scroll-margin-bottom:1px}"
      "{scroll-margin-left:2px;scroll-margin-right:2px}"
      "{scroll-margin:1px 4px 3px 2px}"
      "{scroll-margin-top:1px;scroll-margin-left:1px}"
      "{scroll-margin-bottom:3px;scroll-margin-right:3px}"
      "{scroll-margin-top:1px}"
      "{scroll-margin-left:2px}"
      "{scroll-margin-bottom:3px}"
      "{scroll-margin-right:4px}"
    testDirectionalLogical scrollMargin (px 1) (px 2) (px 3) (px 4)
      "{scroll-margin:1px}"
      "{scroll-margin:logical 1px 2px}"
      "{scroll-margin-block:1px}"
      "{scroll-margin-inline:2px}"
      "{scroll-margin:logical 1px 2px 3px 4px}"
      "{scroll-margin-block-start:1px;scroll-margin-inline-start:1px}"
      "{scroll-margin-block-end:3px;scroll-margin-inline-end:3px}"
      "{scroll-margin-block-start:1px}"
      "{scroll-margin-inline-start:2px}"
      "{scroll-margin-block-end:3px}"
      "{scroll-margin-inline-end:4px}"
  describe "posInset" $ do
    testDirectionalDefault posInset (px 1) (px 2) (px 3) (px 4)
      "{top:1px;left:1px;bottom:1px;right:1px}"
      "{top:1px;left:2px;bottom:1px;right:2px}"
      "{top:1px;bottom:1px}"
      "{left:2px;right:2px}"
      "{top:1px;left:2px;bottom:3px;right:4px}"
      "{top:1px;left:1px}"
      "{bottom:3px;right:3px}"
      "{top:1px}"
      "{left:2px}"
      "{bottom:3px}"
      "{right:4px}"
    testDirectionalLogical posInset (px 1) (px 2) (px 3) (px 4)
      "{inset:1px}"
      "{inset:1px 2px}"
      "{inset-block:1px}"
      "{inset-inline:2px}"
      "{inset:1px 2px 3px 4px}"
      "{inset-block-start:1px;inset-inline-start:1px}"
      "{inset-block-end:3px;inset-inline-end:3px}"
      "{inset-block-start:1px}"
      "{inset-inline-start:2px}"
      "{inset-block-end:3px}"
      "{inset-inline-end:4px}"
  describe "border" $ do
    testDirectionalDefault border (mkBorder (px 1) solid red) (mkBorder (px 2) dotted green) (mkBorder (px 3) dashed blue) (mkBorder (px 4) double yellow)
      "{border:1px solid #ff0000}"
      "{border-top:1px solid #ff0000;border-bottom:1px solid #ff0000;border-left:2px dotted #008000;border-right:2px dotted #008000}"
      "{border-top:1px solid #ff0000;border-bottom:1px solid #ff0000}"
      "{border-left:2px dotted #008000;border-right:2px dotted #008000}"
      "{border-top:1px solid #ff0000;border-left:2px dotted #008000;border-bottom:3px dashed #0000ff;border-right:4px double #ffff00}"
      "{border-top:1px solid #ff0000;border-left:1px solid #ff0000}"
      "{border-bottom:3px dashed #0000ff;border-right:3px dashed #0000ff}"
      "{border-top:1px solid #ff0000}"
      "{border-left:2px dotted #008000}"
      "{border-bottom:3px dashed #0000ff}"
      "{border-right:4px double #ffff00}"
    testDirectionalLogical border (mkBorder (px 1) solid red) (mkBorder (px 2) dotted green) (mkBorder (px 3) dashed blue) (mkBorder (px 4) double yellow)
      "{border:1px solid #ff0000}"
      "{border-block:1px solid #ff0000;border-inline:2px dotted #008000}"
      "{border-block:1px solid #ff0000}"
      "{border-inline:2px dotted #008000}"
      "{border-block-start:1px solid #ff0000;border-inline-start:2px dotted #008000;border-block-end:3px dashed #0000ff;border-inline-end:4px double #ffff00}"
      "{border-block-start:1px solid #ff0000;border-inline-start:1px solid #ff0000}"
      "{border-block-end:3px dashed #0000ff;border-inline-end:3px dashed #0000ff}"
      "{border-block-start:1px solid #ff0000}"
      "{border-inline-start:2px dotted #008000}"
      "{border-block-end:3px dashed #0000ff}"
      "{border-inline-end:4px double #ffff00}"
