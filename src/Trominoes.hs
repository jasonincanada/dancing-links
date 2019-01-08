module Trominoes
  ( trominoTilingLinks
  ) where

import DancingLinks

-- Return the items/options for tiling a chess board with 21 trominoes and 1 monomino
trominoTilingLinks :: Links
trominoTilingLinks = (items, [], options)
  where
    items     = monomino ++ squares
    monomino  = [ it "m" ]
    squares   = [ it $ show r ++ show c | r <- [0..7],
                                          c <- [0..7] ]


    -- m 00
    -- m 01
    -- ..
    -- 00 01 02
    -- 00 10 20
    options    = monominoes ++ trominoes
    
    monominoes = [[it "m", s] | s <- squares ]
    trominoes  = across ++ down
    across     = [[ it $ show row ++ show (col+0),
                    it $ show row ++ show (col+1),
                    it $ show row ++ show (col+2)] | row <- [0..7],
                                                     col <- [0..5] ]

    down       = [[ it $ show (row+0) ++ show col,
                    it $ show (row+1) ++ show col,
                    it $ show (row+2) ++ show col] | row <- [0..5],
                                                     col <- [0..7] ]

    -- Default coloring is no coloring
    it :: String -> Item
    it s = (s, "")
