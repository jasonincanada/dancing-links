module Main where

import Trominoes
import DancingLinks

main :: IO ()
main = do
  table <- linksTableFromFile "inputs/trominoes.links"

  -- Run Algorithm D on the table and output the first solution
  putStrLn $ display $ head $ _solutions $ algorithmD table

{-
    λ> main
    ["00","10","20"]
    ["01","11","21"]
    ["02","12","22"]
    ["03","13","23"]
    ["04","14","24"]
    ["05","15","25"]
    ["06","16","26"]
    ["07","17","27"]
    ["30","40","50"]
    ["60","61","62"]
    ["70","71","72"]
    ["31","41","51"]
    ["32","42","52"]
    ["33","43","53"]
    ["63","64","65"]
    ["73","74","75"]
    ["56","66","76"]
    ["34","44","54"]
    ["35","36","37"]
    ["m","55"]
    ["45","46","47"]
    ["57","67","77"]
-}

display :: Solution -> String
display = unlines . map show

