{- In this revision we start looking into pentomino tiling, various ways to express their
   dimensions and orientations, and accurately counting them. This covers the material on
   pp. 15-18 in [Knuth].  This revision is less about advancing the dancing links code and
   algorithm, and more of a detour into combinatoric counting, with some group theory
   thrown in to leverage symmetries to reduce our search space.

   References:

   Donald Knuth introducing Dancing Links
     https://www.youtube.com/watch?v=_cR9zDlvP88

   [Knuth] The Art of Computer Programming, Vol 4, Pre-fascicle 5C
     https://www-cs-faculty.stanford.edu/~knuth/
-}

module Main where

import Control.Monad (forM_)
import Pentominoes   (countPlacements, Board(..), Tile(..))

-- Dimensions and symmetries of the 12 tiles
tiles = [ Tile 'O' 5 1 True  True  False False
        , Tile 'P' 2 3 False False False False
        , Tile 'Q' 4 2 False False False False
        , Tile 'R' 3 3 False False False False
        , Tile 'S' 4 2 False False False False
        , Tile 'T' 3 3 False True  False False
        , Tile 'U' 3 2 True  False False False
        , Tile 'V' 3 3 False False False True
        , Tile 'W' 3 3 False True  False False
        , Tile 'X' 3 3 True  True  True  False
        , Tile 'Y' 4 2 False False False False
        , Tile 'Z' 3 3 False False True  False
        ]

-- From pp. 16/17 from [Knuth]
expected = [48, 220, 136, 144, 136, 72, 110, 72, 72, 18, 136, 72]

-- Our tiling board has 20 columns and 3 rows
board :: Board
board = Board 20 3

main :: IO ()
main = do
  forM_ tiles $ \tile -> do
    putStrLn $ show (letter tile) ++ ": " ++
               show (countPlacements tile board)

{-
    λ> main
    'O': 48
    'P': 220
    'Q': 136
    'R': 144
    'S': 136
    'T': 72
    'U': 110
    'V': 72
    'W': 72
    'X': 18
    'Y': 136
    'Z': 72

    λ> expected
    [48,220,136,144,136,72,110,72,72,18,136,72]
-}
