{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Animation
(

-- * The animation propery.

  animation
, animations

-- * Animation-delay.

, animationDelay
, animationDelays

-- * Animation-direction.

, AnimationDirection
, animationDirection
, animationDirections
, alternate
, reverse
, alternateReverse

-- * Animation-duration.

, animationDuration
, animationDurations

, IterationCount
, animationIterationCount
, animationIterationCounts
, infinite
, iterationCount

-- * Animation-action-name.

, AnimationName
, animationName

-- * Animation-play-state.

, PlayState
, animationPlayState
, running
, paused

-- * Animation-fill-mode.

, FillMode
, animationFillMode
, forwards
, backwards

-- * Animation-timing-function.

, animationTimingFunction

)
where

import Data.Monoid
import Data.String (IsString)
import Prelude hiding (reverse)

import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Time
import Clay.Transition


animation
  :: Style m
  => AnimationName
  -> Time
  -> TimingFunction
  -> Time
  -> IterationCount
  -> AnimationDirection
  -> FillMode
  -> m ()
animation p de f du i di fm = prefixed (browsers <> "animation") (p ! de ! f ! du ! i ! di ! fm)

animations
  :: Style m
  => [ ( AnimationName
       , Time
       , TimingFunction
       , Time
       , IterationCount
       , AnimationDirection
       , FillMode
       )
     ] -> m ()
animations = prefixed (browsers <> "animation")
            . map (\(p, de, f, du, i, di, fm) -> value (p ! de ! f ! du ! i ! di ! fm))

-------------------------------------------------------------------------------

animationDelay :: Style m => Time -> m ()
animationDelay = prefixed (browsers <> "animation-delay")

animationDelays :: Style m => [Time] -> m ()
animationDelays = prefixed (browsers <> "animation-delay")

-------------------------------------------------------------------------------

newtype AnimationDirection = AnimationDirection Value
  deriving (Val, Other, Normal)

animationDirection :: Style m => AnimationDirection -> m ()
animationDirection = prefixed (browsers <> "animation-direction")

animationDirections :: Style m => [AnimationDirection] -> m ()
animationDirections = prefixed (browsers <> "animation-direction")

alternate, reverse, alternateReverse :: AnimationDirection
alternate        = AnimationDirection "alternate"
reverse          = AnimationDirection "reverse"
alternateReverse = AnimationDirection "alternate-reverse"

-------------------------------------------------------------------------------

animationDuration :: Style m => Time -> m ()
animationDuration = prefixed (browsers <> "animation-duration")

animationDurations :: Style m => [Time] -> m ()
animationDurations = prefixed (browsers <> "animation-duration")

-------------------------------------------------------------------------------

newtype IterationCount = IterationCount Value
  deriving (Val, Other, Normal)

animationIterationCount :: Style m => IterationCount -> m ()
animationIterationCount = prefixed (browsers <> "animation-iteration-count")

animationIterationCounts :: Style m => [IterationCount] -> m ()
animationIterationCounts = prefixed (browsers <> "animation-iteration-count")

infinite :: IterationCount
infinite = IterationCount "infinite"

iterationCount :: Double -> IterationCount
iterationCount = IterationCount . value

-------------------------------------------------------------------------------

newtype AnimationName = AnimationName Value
  deriving (Val, Other, IsString, GlobalValues)

animationName :: Style m => AnimationName -> m ()
animationName = prefixed (browsers <> "animation-name")

-------------------------------------------------------------------------------

newtype PlayState = PlayState Value
  deriving (Val, Other)

animationPlayState :: Style m => PlayState -> m ()
animationPlayState = prefixed (browsers <> "animation-play-state")

running, paused :: PlayState
running = PlayState "running"
paused  = PlayState "paused"

-------------------------------------------------------------------------------

newtype FillMode = FillMode Value
  deriving (Val, Other, None)

animationFillMode :: Style m => FillMode -> m ()
animationFillMode = prefixed (browsers <> "animation-fill-mode")

forwards, backwards :: FillMode
forwards  = FillMode "forwards"
backwards = FillMode "backwards"

-------------------------------------------------------------------------------

animationTimingFunction :: Style m => TimingFunction -> m ()
animationTimingFunction = prefixed (browsers <> "animation-timing-function")
