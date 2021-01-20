{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Mask
(
-- * Generic mask property.

  Mask (mask)

-- * The mask-composite.

, MaskComposite
, clear, copy
, sourceOver, sourceIn, sourceOut, sourceAtop
, destinationOver, destinationIn, destinationOut, destinationAtop
, xor
, maskComposite
, maskComposites

-- * The mask-position.

, maskPosition
, maskPositions

-- * The mask-size.

, maskSize
, maskSizes

-- * The mask-repeat.

, maskRepeat
, maskRepeats

-- * The mask-origin.

, maskOrigin
, maskOrigins

-- * The mask-clip.

, maskClip
, maskClips

-- * The mask-attachment.

, maskAttachment
, maskAttachments

-- * The mask-image.

, maskImage
, maskImages

)
where

import Clay.Background
import Clay.Common
import Clay.Property
import Clay.Stylesheet

pkey :: (Val a, Style m) => Prefixed -> a -> m ()
pkey k = prefixed (browsers <> k)

-------------------------------------------------------------------------------

-- | We implement the generic mask property as a type class that accepts
-- multiple value types. This allows us to combine different mask aspects into
-- a shorthand syntax.

class Val a => Mask a where
  mask :: Style m => a -> m ()
  mask = pkey "mask"

instance Mask a => Mask [a]
instance (Mask a, Mask b) => Mask (a, b)

instance Mask MaskComposite
instance Mask BackgroundPosition
instance Mask BackgroundSize
instance Mask BackgroundRepeat
instance Mask BackgroundOrigin
instance Mask BackgroundClip
instance Mask BackgroundAttachment
instance Mask BackgroundImage

-------------------------------------------------------------------------------

newtype MaskComposite = MaskComposite Value
  deriving (Val, Other, GlobalValues, None)

clear, copy
  , sourceOver, sourceIn, sourceOut, sourceAtop
  , destinationOver, destinationIn, destinationOut, destinationAtop
  , xor :: MaskComposite

clear                = other "clear"
copy                 = other "copy"
sourceOver           = other "source-over"
sourceIn             = other "source-in"
sourceOut            = other "source-out"
sourceAtop           = other "source-atop"
destinationOver      = other "destination-over"
destinationIn        = other "destination-in"
destinationOut       = other "destination-out"
destinationAtop      = other "destination-atop"
xor                  = other "xor"

maskComposite :: Style m => MaskComposite -> m ()
maskComposite = pkey "mask-composite"

maskComposites :: Style m => [MaskComposite] -> m ()
maskComposites = pkey "mask-composite"

-------------------------------------------------------------------------------

maskPosition :: Style m => BackgroundPosition -> m ()
maskPosition = pkey "mask-position"

maskPositions :: Style m => [BackgroundPosition] -> m ()
maskPositions = pkey "mask-position"

-------------------------------------------------------------------------------

maskSize :: Style m => BackgroundSize -> m ()
maskSize = pkey "mask-size"

maskSizes :: Style m => [BackgroundSize] -> m ()
maskSizes = pkey "mask-size"

-------------------------------------------------------------------------------

maskRepeat :: Style m => BackgroundRepeat -> m ()
maskRepeat = pkey "mask-repeat"

maskRepeats :: Style m => [BackgroundRepeat] -> m ()
maskRepeats = pkey "mask-repeat"

-------------------------------------------------------------------------------

maskImage :: Style m => BackgroundImage -> m ()
maskImage = pkey "mask-image"

maskImages :: Style m => [BackgroundImage] -> m ()
maskImages = pkey "mask-image"

-------------------------------------------------------------------------------

maskOrigin :: Style m => BackgroundOrigin -> m ()
maskOrigin = pkey "mask-origin"

maskOrigins :: Style m => [BackgroundOrigin] -> m ()
maskOrigins = pkey "mask-origin"

-------------------------------------------------------------------------------

maskClip :: Style m => BackgroundClip -> m ()
maskClip = pkey "mask-clip"

maskClips :: Style m => [BackgroundClip] -> m ()
maskClips = pkey "mask-clip"

-------------------------------------------------------------------------------

maskAttachment :: Style m => BackgroundAttachment -> m ()
maskAttachment = pkey "mask-attachment"

maskAttachments :: Style m => [BackgroundAttachment] -> m ()
maskAttachments = pkey "mask-attachment"
