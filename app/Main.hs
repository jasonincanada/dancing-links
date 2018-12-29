module Main where

import DancingLinks
import NanoParsec

file = "inputs/table10.links"

main :: IO ()
main = do
  contents <- readFile file
  let links = run parseLinksFile contents
  let table = tableFromLinks links

  -- cover(i) and uncover(i) are inverse operations, this should be True
  putStrLn $ show $ table == (uncover 1 $ cover 1 table)

  -- Run Algorithm D on the table and output the final state
  putStrLn $ show $ algorithmD table

  -- ... _solutions = [[["a","d","f"],["b","g"],["c","e"]]]}
  --
  -- The options "a d f", "b g", and "c e" form the single solution to
  -- the exact cover algorithm run on table10.links
