module Sudoku
  ( SudokuPuzzle(..)
  , sudokuFromFile
  , sudokuOptions
  ) where

import Control.Applicative (many)
import Control.Category    ((>>>))
import Data.List           ((\\))
import NanoParsec hiding   (item)

type Row   = Int
type Col   = Int
type Digit = Int

type SudokuPuzzle = [(Row, Col, Digit)]

-- 1,7 2
puzzleLine :: Parser (Row, Col, Digit)
puzzleLine = (,,) <$> number <* comma
                  <*> number <* space
                  <*> number <* nl

comma = char ','
nl    = char '\n'

sudokuFile :: Parser SudokuPuzzle
sudokuFile = many puzzleLine

sudokuFromFile :: FilePath -> IO SudokuPuzzle
sudokuFromFile file = readFile file >>= (run sudokuFile >>> return)

-- Get the options needed for the exact cover algorithm for a partial sudoku puzzle
sudokuOptions :: SudokuPuzzle -> [[String]]
sudokuOptions puzzle = all \\ concatMap ruledOut puzzle
  where
    all   = [[ item 'p' i j, item 'r' i k, item 'c' j k, item 'b' (box i j) k ]
               | i <- [0..8],
                 j <- [0..8],
                 k <- [1..9]]

    -- Which options are ruled out when we know a digit in a cell
    ruledOut :: (Row, Col, Digit) -> [[String]]
    ruledOut (row, col, digit) = cell ++ across ++ down ++ inbox
      where
        cell   = [[ item 'p' row col, item 'r' row k    , item 'c' col k    , item 'b' (box row col) k   ] | k <- [1..9] ]
        across = [[ item 'p' row j  , item 'r' row digit, item 'c' j   digit, item 'b' (box row j) digit ] | j <- [0..8] ]
        down   = [[ item 'p' i   col, item 'r' i   digit, item 'c' col digit, item 'b' (box i col) digit ] | i <- [0..8] ]
        inbox  = [[ item 'p' i   j  , item 'r' i   digit, item 'c' j   digit, item 'b' (box i j)   digit ]
                    | i <- [0..8],
                      j <- [0..8],
                      box i j == box row col ]

    -- The 9x9 box number for a coordinate
    box :: Row -> Col -> Int
    box row col = 3*(row `div` 3) + (col `div` 3)

    item :: Char -> Int -> Int -> String
    item c m n = c : show m ++ show n
