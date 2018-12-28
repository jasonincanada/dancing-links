module Main where

import DancingLinks
import NanoParsec
import qualified Data.IntMap as IntMap

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

  -- ... _solutions = [[22,25,10]]}
  --
  -- The options containing nodes 22, 25, and 10 form the single solution to
  -- the exact cover algorithm run on table10.links
