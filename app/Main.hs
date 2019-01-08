module Main where

import DancingLinks

main :: IO ()
main = do
  table <- linksTableFromFile "inputs/xcc.links"

  -- Run Algorithm D on the table and output the first solution
  putStrLn $ display $ head $ _solutions $ algorithmD table

  {-
      λ> main
      q x:A
      p r x:A y
  -}

display :: [Option] -> String
display = unlines . map (unwords . map showItem)

