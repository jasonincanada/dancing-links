module Main where

import DancingLinks
import NanoParsec

file = "inputs/table10.links"

main :: IO ()
main = do
  contents <- readFile file
  let links = run parseLinksFile contents

  let table     = tableFromLinks links
  let covered   = cover 1 table
  let uncovered = uncover 1 covered

  -- These should be identical
  putStrLn $ show table
  putStrLn $ show uncovered
