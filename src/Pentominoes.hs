{-# Language RecordWildCards #-}

module Pentominoes ( countPlacements
                   , Tile(..)
                   , Board(..)
                   ) where

import Data.Bool (bool)

type Width  = Int
type Height = Int

-- I think I can do this with 3 symmetry arguments instead of 4 since the rotSym and
-- diagSym values are never both True at the same time, but I'm still thinking this over
data Tile = Tile { letter   :: Char
                 , width    :: Width
                 , height   :: Height
                 , vertSym  :: Bool
                 , horizSym :: Bool
                 , rotSym   :: Bool
                 , diagSym  :: Bool
                 } deriving (Show)

data Board = Board Width Height


countPlacements :: Tile -> Board -> Int
countPlacements Tile{..} (Board cols rows) = (r0 + r1) `div` symmetries
  where
    r0 = 2 * 2 * -- Account for horiz/vertical flips
         (rows - (height - 1)) *
         (cols - (width  - 1))

    -- The same but rotated 90 degrees
    r1 = 2 * 2 *
         (rows - (width  - 1)) *
         (cols - (height - 1)) *
         bool 0 1 fitsWhenRotated

    fitsWhenRotated :: Bool
    fitsWhenRotated = width <= rows

    symmetries = 2 ^ (bool 0 1 vertSym  +
                      bool 0 1 horizSym +
                      bool 0 1 rotSym   +
                      bool 0 1 diagSym)

