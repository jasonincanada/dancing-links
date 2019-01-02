module Main where

import Data.List (sortBy)
import Data.Ord  (comparing)
import DancingLinks

-- Advent of Code 2018 Day 16 Part 2 can be formulated in terms of an exact
-- cover problem.  The items are the 16 opcode names along with op0..op15. The
-- options are all the possible opcode/number assignments.
--
-- See: https://github.com/jasonincanada/aoc-2018/blob/master/src/Day16.hs#L149

main :: IO ()
main = do
  table <- linksTableFromFile "inputs/advent-16.links"

  -- uncover(i) is the inverse of cover(i), this should be True
  print $ (uncover 1 . cover 1) table == table

  -- Run Algorithm D on the table and output the solutions
  mapM_ putStr $ map display $ _solutions (algorithmD table)

{-  λ> main
    True
    op0 -> eqri
    op1 -> bani
    op2 -> seti
    op3 -> bori
    op4 -> eqir
    op5 -> banr
    op6 -> borr
    op7 -> muli
    op8 -> setr
    op9 -> addr
    op10 -> eqrr
    op11 -> addi
    op12 -> gtir
    op13 -> gtrr
    op14 -> gtri
    op15 -> mulr
-}

display :: Solution -> String
display = unlines
            . map (\(a:b:_) -> b ++ " -> " ++ a)
            . sortBy (comparing (\(a:b:_) -> read (tail $ tail b) :: Int))

