module Main where

import Data.List (nub, sort)
import DancingLinks
import NanoParsec
import Sudoku

--file = "inputs/table10.links"
--file = "inputs/langford-pairs-3.links"

sudokuInput :: SudokuPuzzle -> ([Item], [Option])
sudokuInput puzzle = (items options, options)
  where
    items   = sort . nub . concat
    options = sudokuOptions puzzle

main :: IO ()
main = do
  puzzle <- sudokuFromFile "inputs/newspaper.sudoku"
  let input = sudokuInput puzzle
  let table = tableFromLinks input

  -- uncover(i) is the inverse of cover(i), this should be True
  print $ (uncover 1 . cover 1) table == table

  -- Run Algorithm D on the table and output the solutions
  print $ map sort $ _solutions (algorithmD table)

