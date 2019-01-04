module NQueens
  ( nQueensOptions
  ) where

nQueensOptions :: Int -> [[String]]
nQueensOptions n = cells ++ slack
  where
    cells = [ option i j (i+j) (i-j) | i <- [1..n],
                                       j <- [1..n] ]

    slack = [ ["a" ++ show s] | s <- [2..2*n]       ] ++
            [ ["b" ++ show d] | d <- [(1-n)..(n-1)] ]

option :: Int -> Int -> Int -> Int -> [String]
option r c a b = [ "r" ++ show r,
                   "c" ++ show c,
                   "a" ++ show a,
                   "b" ++ show b ]
