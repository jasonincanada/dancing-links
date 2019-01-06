module Trominoes
  ( trominoTilingLinks
  ) where

import DancingLinks

-- Return the items/options for tiling a chess board with 21 trominoes and 1 monomino
trominoTilingLinks :: Links
trominoTilingLinks = (items, [], options)
  where
    items     = monomino ++ squares
    monomino  = [ "m" ]
    squares   = [ show r ++ show c | r <- [0..7],
                                     c <- [0..7] ]


    -- m 00
    -- m 01
    -- ..
    -- 00 01 02
    -- 00 10 20
    options    = monominoes ++ trominoes
    
    monominoes = [["m", s] | s <- squares ] 
    trominoes  = across ++ down
    across     = [[ show row ++ show (col+0),
                    show row ++ show (col+1),
                    show row ++ show (col+2)] | row <- [0..7],
                                                col <- [0..5] ]

    down       = [[ show (row+0) ++ show col,
                    show (row+1) ++ show col,
                    show (row+2) ++ show col] | row <- [0..5],
                                                col <- [0..7] ]

