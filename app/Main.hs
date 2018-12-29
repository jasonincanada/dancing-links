module Main where

import Data.List (intercalate)
import DancingLinks
import NanoParsec

--file = "inputs/table10.links"
file = "inputs/langford-pairs-3.links"

-- Let's do the Langford pairs problem for n = 3 (p. 6)
langfordInput :: Int -> ([Item], [Option])
langfordInput n = (items, options)
  where
    items   = map show [1..n] ++ [ 's' : show i | i <- [1 .. 2*n] ]
    options = [ [show i, 's':show j, 's':show k]
                  | i <- [1..n],
                    j <- [1..2*n-1],
                    let k = i+j+1,
                    k <= 2*n ]

-- I ran the following command once in the repl to create inputs/langford-pairs-3.links
--
-- λ> writeLinksFile "langford-pairs-3" (langfordInput 3)
writeLinksFile :: String -> ([Item], [Option]) -> IO ()
writeLinksFile name (items, options) = writeFile path string
  where
    path = "inputs/" ++ name ++ ".links"

    string = unlines [ unwords items,
                       "",
                       intercalate "\n" $ map unwords options ]

main :: IO ()
main = do
  contents <- readFile file
  let links = run parseLinksFile contents
  let table = tableFromLinks links

  -- cover(i) and uncover(i) are inverse operations, this should be True
  print $ table == uncover 1 (cover 1 table)

  -- Run Algorithm D on the table and output the final state
  print $ algorithmD table

  -- ... _solutions = [[["3","s2","s6"],["1","s3","s5"],["2","s1","s4"]],[["3","s1","s5"],["2","s3","s6"],["1","s2","s4"]]]}
  --
  -- There are two solutions to the Langford pairs problem for n = 3:
  --
  -- 1 s3 s5
  -- 2 s1 s4
  -- 3 s2 s6
  --
  -- and
  --
  -- 1 s2 s4
  -- 2 s3 s6
  -- 3 s1 s5
