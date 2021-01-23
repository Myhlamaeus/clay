{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clay.Directional where

import Clay.Common
import Control.Monad (join)
import Data.These

-- | A value which can be specified per axis (block and/or inline.)
newtype Axial a = Axial { unAxial :: These a a }
  deriving (Show, Eq, Ord)

instance Functor Axial where
  fmap f (Axial (This a)) = Axial $ This $ f a
  fmap f (Axial (That b)) = Axial $ That $ f b
  fmap f (Axial (These a b)) = Axial $ These (f a) (f b)

instance Other a => Other (Axial a) where other = all' . other
instance All a => All (Axial a) where all = all' Clay.Common.all
instance Auto a => Auto (Axial a) where auto = all' auto
instance Baseline a => Baseline (Axial a) where baseline = all' baseline
instance Center a => Center (Axial a) where center = all' center
instance None a => None (Axial a) where none = all' none
instance Normal a => Normal (Axial a) where normal = all' normal
instance Visible a => Visible (Axial a) where visible = all' visible
instance Hidden a => Hidden (Axial a) where hidden = all' hidden

instance GlobalValues a => GlobalValues (Axial a) where
  inherit = all' inherit
  initial = all' inherit
  revert = all' inherit
  unset = all' inherit

-- | Set the value for both axes
-- >>> render $ padding $ allDirs $ px 5
-- padding: 5px;
--
-- >>> renderLogical $ padding $ allDirs $ px 5
-- padding: 5px;
all' :: a -> Axial a
all' = Axial . join These

block', inline' :: a -> Axial a

-- | Set the value for the block axis/the block-start and block-end directions
-- >>> render $ padding $ block' $ px 5
-- padding-top: 5px;
-- padding-bottom: 5px;
--
-- >>> renderLogical $ padding $ block' $ px 5
-- padding-block: 5px;
block' = Axial . This

-- | Set the value for the inline axis/the inline-start and inline-end directions
-- >>> render $ padding $ inline' $ px 5
-- padding-left: 5px;
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ inline' $ px 5
-- padding-inline: 5px;
inline' = Axial . That

-- | Set one value per axis
-- >>> render $ padding $ eachAxis (px 5) (px 10)
-- padding: 5px 10px;
--
-- >>> renderLogical $ padding $ eachAxis (px 5) (px 10)
-- padding: logical 5px 10px;
eachAxis :: a -> a -> Axial a
eachAxis b i = Axial $ These b i

-------------------------------------------------------------------------------

-- | A value which can be specified per direction (any non-empty combination of block-start, inline-start, block-end, inline-end)
newtype Directional a = Directional { unDirectional :: These (Axial a) (Axial a) }
  deriving (Show, Eq, Ord)

instance Functor Directional where
  fmap f (Directional (This a)) = Directional $ This $ f <$> a
  fmap f (Directional (That b)) = Directional $ That $ f <$> b
  fmap f (Directional (These a b)) = Directional $ These (f <$> a) (f <$> b)

instance Other a => Other (Directional a) where other = allDirs . other
instance All a => All (Directional a) where all = allDirs Clay.Common.all
instance Auto a => Auto (Directional a) where auto = allDirs auto
instance Baseline a => Baseline (Directional a) where baseline = allDirs baseline
instance Center a => Center (Directional a) where center = allDirs center
instance None a => None (Directional a) where none = allDirs none
instance Normal a => Normal (Directional a) where normal = allDirs normal
instance Visible a => Visible (Directional a) where visible = allDirs visible
instance Hidden a => Hidden (Directional a) where hidden = allDirs hidden

instance GlobalValues a => GlobalValues (Directional a) where
  inherit = allDirs inherit
  initial = allDirs initial
  revert = allDirs revert
  unset = allDirs unset

-- | Set the value for all directions
-- >>> render $ padding $ allDirs $ px 5
-- padding: 5px;
--
-- >>> renderLogical $ padding $ allDirs $ px 5
-- padding: 5px;
allDirs :: a -> Directional a
allDirs = bothSides . all'

-- | Set one value per side (start and end)
-- >>> render $ padding $ eachSide (all' (px 5)) (inline' (px 10))
-- padding-top: 5px;
-- padding-left: 5px;
-- padding-right: 10px;
--
-- >>> renderLogical $ padding $ eachSide (all' (px 5)) (inline' (px 10))
-- padding-block-start: 5px;
-- padding-inline: 5px 10px;
eachSide :: Axial a -> Axial a -> Directional a
eachSide s e = Directional $ These s e

-- | Set one value for both sides (start and end)
-- >>> render $ padding $ bothSides (inline' (px 5))
-- padding-left: 5px;
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ bothSides (inline' (px 5))
-- padding-inline: 5px;
bothSides :: Axial a -> Directional a
bothSides a = eachSide a a

-- | Set one value per direction
-- >>> render $ padding $ eachDir (px 5) (px 10) (px 15) (px 20)
-- padding: 5px 20px 15px 10px;
--
-- >>> renderLogical $ padding $ eachAxis (px 5) (px 10) (px 15) (px 20)
-- padding: logical 5px 10px 15px 20px;
eachDir :: a -> a -> a -> a -> Directional a
eachDir bs is be ie = eachSide (eachAxis bs is) (eachAxis be ie)

start', end' :: Axial a -> Directional a

-- | Set the value for the start side/the block-start and inline-start directions
-- >>> render $ padding $ start' $ all' $ px 5
-- padding-top: 5px;
-- padding-left: 5px;
--
-- >>> renderLogical $ padding $ start' $ all' $ px 5
-- padding-block-start: 5px;
-- padding-inline-start: 5px;
start' = Directional . This

-- | Set the value for the end side/the block-end and inline-end directions
-- >>> render $ padding $ end' $ all' $ px 5
-- padding-bottom: 5px;
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ end' $ all' $ px 5
-- padding-block-end: 5px;
-- padding-inline-end: 5px;
end' = Directional . That

blockStart, blockEnd, inlineStart, inlineEnd :: a -> Directional a

-- | Set the value for the block-start direction
-- >>> render $ padding $ blockStart $ px 5
-- padding-top: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-block-start: 5px;
blockStart = start' . block'

-- | Set the value for the block-end direction
-- >>> render $ padding $ blockEnd $ px 5
-- padding-bottom: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-block-end: 5px;
blockEnd = end' . block'

-- | Set the value for the inline-start direction
-- >>> render $ padding $ inlineStart $ px 5
-- padding-left: 5px;
--
-- >>> renderLogical $ padding $ start' $ px 5
-- padding-inline-start: 5px;
inlineStart = start' . inline'

-- | Set the value for the inline-end direction
-- >>> render $ padding $ inlineEnd $ px 5
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-inline-end: 5px;
inlineEnd = end' . inline'

-------------------------------------------------------------------------------

-- | A Directional rotated by 1/8 clockwise (in horizontal-tb)
newtype CornerDirectional a = CornerDirectional (Directional a)
  deriving (Show, Eq, Functor)
  deriving newtype (IsDirectional)

-- | Set the value for all corner directions
-- >>> render $ borderRadius $ allCorners $ px 5
-- border-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ allCorners $ px 5
-- border-radius: 5px;
allCorners :: a -> CornerDirectional a
allCorners = CornerDirectional . allDirs

startStart, startEnd, endEnd, endStart :: a -> CornerDirectional a

eachCorner :: a -> a -> a -> a -> CornerDirectional a

-- | Set the value for each corner directions
-- >>> render $ borderRadius $ eachCorner (px 1) (px 2) (px 3) (px 4)
-- border-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ eachCorner $ px 5
-- border-radius: 5px;
eachCorner ss es ee se = CornerDirectional $ eachDir ss es ee se

-- | Set the value for the start-start direction
-- >>> render $ borderRadius $ startStart $ px 5
-- border-top-left-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ startStart $ px 5
-- padding-start-start-radius: 5px;
startStart = CornerDirectional . blockStart

-- | Set the value for the end-start direction
-- >>> render $ borderRadius $ endStart $ px 5
-- border-top-right-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ endStart $ px 5
-- padding-end-start-radius: 5px;
endStart = CornerDirectional . blockEnd

-- | Set the value for the end-end direction
-- >>> render $ borderRadius $ endEnd $ px 5
-- border-bottom-right-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ endEnd $ px 5
-- padding-end-end-radius: 5px;
endEnd = CornerDirectional . inlineEnd

-- | Set the value for the start-end direction
-- >>> render $ borderRadius $ startEnd $ px 5
-- border-bottom-left-radius: 5px;
--
-- >>> renderLogical $ borderRadius $ startEnd $ px 5
-- padding-start-end-radius: 5px;
startEnd = CornerDirectional . inlineStart

-------------------------------------------------------------------------------

class IsDirectional dir where
  toDirectional :: dir a -> Directional a
  flipAxes :: dir a -> dir a
  flipOverBlock :: dir a -> dir a
  flipOverInline :: dir a -> dir a

instance IsDirectional Axial where
  toDirectional = bothSides
  flipAxes (Axial (This b)) = Axial $ That b
  flipAxes (Axial (That i)) = Axial $ This i
  flipAxes (Axial (These b i)) = Axial $ These i b
  flipOverBlock = id
  flipOverInline = id

instance IsDirectional Directional where
  toDirectional = id
  flipAxes (Directional (This b)) = Directional $ This $ flipAxes b
  flipAxes (Directional (That i)) = Directional $ That $ flipAxes i
  flipAxes (Directional (These b i)) = Directional $ These (flipAxes b) (flipAxes i)
  -- bs is be ie -> bs ie be is
  flipOverBlock (Directional (This (Axial (That is)))) = inlineEnd is
  flipOverBlock (Directional (That (Axial (That ie)))) = inlineStart ie
  flipOverBlock (Directional (These (Axial (This bs)) (Axial (That ie)))) = start' (eachAxis bs ie)
  flipOverBlock (Directional (These (Axial (This bs)) (Axial (These be ie)))) = eachSide (eachAxis bs ie) (block' be)
  flipOverBlock (Directional (These (Axial (That is)) (Axial (That ie)))) = eachSide (inline' ie) (inline' is)
  flipOverBlock (Directional (These (Axial (That is)) (Axial (These be ie)))) = eachSide (inline' ie) (eachAxis be is)
  flipOverBlock (Directional (These (Axial (These bs is)) (Axial (That ie)))) = eachSide (eachAxis bs ie) (inline' is)
  flipOverBlock (Directional (These (Axial (These bs is)) (Axial (These be ie)))) = eachDir bs ie be is
  flipOverBlock a = a
  -- bs is be ie -> be is bs ie
  flipOverInline = flipAxes . flipOverBlock . flipAxes
