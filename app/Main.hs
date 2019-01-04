module Main where

import DancingLinks

main :: IO ()
main = do
  table <- linksTableFromFile "inputs/nqueens-4.links"

  -- Run Algorithm D on the table and output the solutions
  mapM_ putStr $ map display $ _solutions $ algorithmD table

display :: Solution -> String
display = unlines . map show

