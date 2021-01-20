{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Background
(
-- * Generic background property.

  Background (background)

-- * The background-color.

, backgroundColor

-- * The background-position.

, BackgroundPosition
, backgroundPosition
, backgroundPositions
, placed
, positioned

-- * The background-size.

, BackgroundSize
, backgroundSize
, backgroundSizes
, contain, cover
, by

-- * The background-repeat.

, BackgroundRepeat
, backgroundRepeat
, backgroundRepeats
, repeat, space, round, noRepeat
, xyRepeat
, repeatX, repeatY

-- * The background-origin.

, BackgroundOrigin
, backgroundOrigin
, backgroundOrigins
, origin

-- * The background-clip.

, BackgroundClip
, backgroundClip
, backgroundClips
, boxClip

-- * The background-attachment.

, BackgroundAttachment
, backgroundAttachment
, backgroundAttachments
, attachFixed, attachScroll

-- * The background-image.

, BackgroundImage
, backgroundImage
, backgroundImages
, url

-- * Specifying sides.

, Side
, sideTop
, sideLeft
, sideRight
, sideBottom
, sideCenter
, sideMiddle

-- * Specifying directions and location.

, Direction
, straight
, angular

, Location
, Loc
, Val
, location
)
where

import Data.Text (Text)
import Data.Monoid
import Prelude hiding (repeat, round)

import Clay.Box
import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-- | We implement the generic background property as a type class that accepts
-- multiple value types. This allows us to combine different background aspects
-- into a shorthand syntax.

class Val a => Background a where
  background :: Style m => a -> m ()
  background = key "background"

instance Background a => Background [a]
instance (Background a, Background b) => Background (a, b)

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background BackgroundOrigin
instance Background BackgroundClip
instance Background BackgroundAttachment
instance Background BackgroundImage

-------------------------------------------------------------------------------

backgroundColor :: Style m => Color -> m ()
backgroundColor = key "background-color"

-------------------------------------------------------------------------------

newtype BackgroundPosition = BackgroundPosition Value
  deriving (Val, Other, GlobalValues)

placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition (value (a, b))

positioned :: Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition (value (a, b))

backgroundPosition :: Style m => BackgroundPosition -> m ()
backgroundPosition = key "background-position"

backgroundPositions :: Style m => [BackgroundPosition] -> m ()
backgroundPositions = key "background-position"

-------------------------------------------------------------------------------

newtype BackgroundSize = BackgroundSize Value
  deriving (Val, Other, GlobalValues)

instance Auto BackgroundSize where auto = auto `by` auto

contain, cover :: BackgroundSize

contain = BackgroundSize "contain"
cover   = BackgroundSize "cover"

by :: Size a -> Size b -> BackgroundSize
by a b = BackgroundSize (value (a, b))

backgroundSize :: Style m => BackgroundSize -> m ()
backgroundSize = key "background-size"

backgroundSizes :: Style m => [BackgroundSize] -> m ()
backgroundSizes = key "background-size"

-------------------------------------------------------------------------------

newtype BackgroundRepeat = BackgroundRepeat Value
  deriving (Val, Other, GlobalValues, None)

repeat, space, round, noRepeat :: BackgroundRepeat

repeat   = BackgroundRepeat "repeat"
space    = BackgroundRepeat "space"
round    = BackgroundRepeat "round"
noRepeat = BackgroundRepeat "no-repeat"

xyRepeat :: BackgroundRepeat -> BackgroundRepeat -> BackgroundRepeat
xyRepeat a b = BackgroundRepeat (value (a, b))

repeatX, repeatY :: BackgroundRepeat

repeatX = xyRepeat repeat noRepeat
repeatY = xyRepeat noRepeat repeat

backgroundRepeat :: Style m => BackgroundRepeat -> m ()
backgroundRepeat = key "background-repeat"

backgroundRepeats :: Style m => [BackgroundRepeat] -> m ()
backgroundRepeats = key "background-repeat"

-------------------------------------------------------------------------------

newtype BackgroundImage = BackgroundImage Value
  deriving (Val, Other, GlobalValues, None)

url :: Text -> BackgroundImage
url u = BackgroundImage (value ("url(\"" <> u <> "\")"))

backgroundImage :: Style m => BackgroundImage -> m ()
backgroundImage = key "background-image"

backgroundImages :: Style m => [BackgroundImage] -> m ()
backgroundImages = key "background-image"

-------------------------------------------------------------------------------

newtype BackgroundOrigin = BackgroundOrigin Value
  deriving (Val, Other, GlobalValues)

origin :: BoxType -> BackgroundOrigin
origin b = BackgroundOrigin (value b)

backgroundOrigin :: Style m => BackgroundOrigin -> m ()
backgroundOrigin = key "background-origin"

backgroundOrigins :: Style m => [BackgroundOrigin] -> m ()
backgroundOrigins = key "background-origin"

-------------------------------------------------------------------------------

newtype BackgroundClip = BackgroundClip Value
  deriving (Val, Other, GlobalValues)

boxClip :: BoxType -> BackgroundClip
boxClip b = BackgroundClip (value b)

backgroundClip :: Style m => BackgroundClip -> m ()
backgroundClip = key "background-clip"

backgroundClips :: Style m => [BackgroundClip] -> m ()
backgroundClips = key "background-clip"

-------------------------------------------------------------------------------

newtype BackgroundAttachment = BackgroundAttachment Value
  deriving (Other, Val, GlobalValues)

attachFixed, attachScroll :: BackgroundAttachment
attachFixed  = BackgroundAttachment "fixed"
attachScroll = BackgroundAttachment "scroll"

backgroundAttachment :: Style m => BackgroundAttachment -> m ()
backgroundAttachment = key "background-attachment"

backgroundAttachments :: Style m => [BackgroundAttachment] -> m ()
backgroundAttachments = key "background-attachment"

-------------------------------------------------------------------------------

newtype Side = Side Value
  deriving (Val, Other, GlobalValues)

-- | We have to prefix these values to avoid conflict with existing property
-- names.

sideTop, sideLeft, sideRight, sideBottom, sideCenter, sideMiddle :: Side

sideTop    = Side "top"
sideLeft   = Side "left"
sideRight  = Side "right"
sideBottom = Side "bottom"
sideCenter = Side "center"
sideMiddle = Side "middle"

-------------------------------------------------------------------------------

newtype Direction = Direction Value
  deriving (Val, Other)

straight :: Side -> Direction
straight a = Direction (value a)

angular :: Angle a -> Direction
angular a = Direction (value a)

newtype Location = Location Value
  deriving (Val, Other)

class Val a => Loc a where
  location :: a -> Location
  location = Location . value

instance Loc Side
instance Loc (Size a)
instance (Loc a, Loc b) => Loc (a, b)
