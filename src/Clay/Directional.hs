module Clay.Directional where

import Clay.Common

-- | A value which can be specified per axis (block and/or inline.)
data Axial a
  = AxialBoth a -- ^ The same value for both axes
  | AxialEach a a -- ^ One value per axis
  | AxialBlock a -- ^ Only a value for the block axis
  | AxialInline a -- ^ Only a value for the inline axis
  deriving (Show, Eq)

instance Functor Axial where
  fmap f (AxialBoth a) = AxialBoth $ f a
  fmap f (AxialEach b i) = AxialEach (f b) (f i)
  fmap f (AxialBlock b) = AxialBlock (f b)
  fmap f (AxialInline i) = AxialInline (f i)

instance Other a => Other (Axial a) where other = AxialBoth . other
instance All a => All (Axial a) where all = AxialBoth Clay.Common.all
instance Auto a => Auto (Axial a) where auto = AxialBoth auto
instance Baseline a => Baseline (Axial a) where baseline = AxialBoth baseline
instance Center a => Center (Axial a) where center = AxialBoth center
instance None a => None (Axial a) where none = AxialBoth none
instance Normal a => Normal (Axial a) where normal = AxialBoth normal
instance Visible a => Visible (Axial a) where visible = AxialBoth visible
instance Hidden a => Hidden (Axial a) where hidden = AxialBoth hidden
instance GlobalValues a => GlobalValues (Axial a) where
  inherit = AxialBoth inherit
  initial = AxialBoth initial
  revert = AxialBoth revert
  unset = AxialBoth unset

all', block', inline' :: a -> Axial a

-- | Set the value for both axes/all directions
-- >>> render $ padding $ all' $ px 5
-- padding: 5px;
--
-- >>> renderLogical $ padding $ all' $ px 5
-- padding: 5px;
all' = AxialBoth

-- | Set the value for the block axis/the block-start and block-end directions
-- >>> render $ padding $ block' $ px 5
-- padding-top: 5px;
-- padding-bottom: 5px;
--
-- >>> renderLogical $ padding $ block' $ px 5
-- padding-block: 5px;
block' = AxialBlock

-- | Set the value for the inline axis/the inline-start and inline-end directions
-- >>> render $ padding $ inline' $ px 5
-- padding-left: 5px;
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ inline' $ px 5
-- padding-inline: 5px;
inline' = AxialInline

-- | Set one value per axis
-- >>> render $ padding $ eachAxis (px 5) (px 10)
-- padding: 5px 10px;
--
-- >>> renderLogical $ padding $ eachAxis (px 5) (px 10)
-- padding: logical 5px 10px;
eachAxis :: a -> a -> Axial a
eachAxis = AxialEach

-- | A value which can be specified per direction (any non-empty combination of block-start, inline-start, block-end, inline-end)
data Directional a
  = DirectionalEach (Axial a) (Axial a) -- ^ One value per side (start and end)
  | DirectionalStart (Axial a) -- ^ Only a value for the start side
  | DirectionalEnd (Axial a) -- ^ Only a value for the end side
  deriving (Show, Eq)

instance Functor Directional where
  fmap f (DirectionalEach s e) = DirectionalEach (f <$> s) (f <$> e)
  fmap f (DirectionalStart s) = DirectionalStart (f <$> s)
  fmap f (DirectionalEnd e) = DirectionalEnd (f <$> e)

-- | Set one value per direction
-- >>> render $ padding $ eachDir (px 5) (px 10) (px 15) (px 20)
-- padding: 5px 20px 15px 10px;
--
-- >>> renderLogical $ padding $ eachAxis (px 5) (px 10) (px 15) (px 20)
-- padding: logical 5px 10px 15px 20px;
eachDir :: a -> a -> a -> a -> Directional a
eachDir bs is be ie = DirectionalEach (AxialEach bs is) (AxialEach be ie)

start', end' :: a -> Directional a

-- | Set the value for the start side/the block-start and inline-start directions
-- >>> render $ padding $ start' $ px 5
-- padding-top: 5px;
-- padding-left: 5px;
--
-- >>> renderLogical $ padding $ start' $ px 5
-- padding-block-start: 5px;
-- padding-inline-start: 5px;
start' = DirectionalStart . AxialBoth

-- | Set the value for the end side/the block-end and inline-end directions
-- >>> render $ padding $ end' $ px 5
-- padding-bottom: 5px;
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-block-end: 5px;
-- padding-inline-end: 5px;
end' = DirectionalEnd . AxialBoth

blockStart, blockEnd, inlineStart, inlineEnd :: a -> Directional a

-- | Set the value for the block-start direction
-- >>> render $ padding $ blockStart $ px 5
-- padding-top: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-block-start: 5px;
blockStart = DirectionalStart . AxialBlock

-- | Set the value for the block-end direction
-- >>> render $ padding $ blockEnd $ px 5
-- padding-bottom: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-block-end: 5px;
blockEnd = DirectionalEnd . AxialBlock

-- | Set the value for the inline-start direction
-- >>> render $ padding $ inlineStart $ px 5
-- padding-left: 5px;
--
-- >>> renderLogical $ padding $ start' $ px 5
-- padding-inline-start: 5px;
inlineStart = DirectionalStart . AxialInline

-- | Set the value for the inline-end direction
-- >>> render $ padding $ inlineEnd $ px 5
-- padding-right: 5px;
--
-- >>> renderLogical $ padding $ end' $ px 5
-- padding-inline-end: 5px;
inlineEnd = DirectionalEnd . AxialInline
