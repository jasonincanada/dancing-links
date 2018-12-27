module Main where

import DancingLinks
import NanoParsec

file = "inputs/table10.links"

main :: IO ()
main = do
  contents <- readFile file
  let links = run parseLinksFile contents
  let table = tableFromLinks links
  putStrLn $ show table
