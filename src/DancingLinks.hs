{- DancingLinks.hs

   This is a Haskell implementation of Donald Knuth's Dancing Links. A draft of his upcoming
   installment of The Art of Computer Programming containing descriptions of the Dancing
   Links structure and algorithms is online (see references). 

   The code implemented below and executed with the inputs/table10.links file parses, loads,
   and exactly reproduces table (10) in Knuth (p. 3). Calling "main" will print out the
   values in Table 1 on (p. 4).

   So it is not too useful yet but this is the initial version that loads in and understands
   the format of a *.links file.

   This revision implements cover(i), hide(i), uncover(i), unhide(i) [operations (12)-(15)]


   References:

   Donald Knuth introducing Dancing Links
     https://www.youtube.com/watch?v=_cR9zDlvP88

   [Knuth] The Art of Computer Programming, Vol 4, Pre-fascicle 5C
     https://www-cs-faculty.stanford.edu/~knuth/
-}

{-# Language TemplateHaskell #-}

module DancingLinks
    ( parseLinksFile,
      tableFromLinks,
      cover,
      uncover
    ) where

import           Control.Applicative (some, many)
import           Control.Lens
import qualified Data.IntMap as IntMap
import           Data.Char (isAlphaNum)
import           Data.List (sort)
import           NanoParsec hiding (item, option)


----------------
-- Types -------
----------------

type Item      = String
type Option    = [Item]
type Solution  = [Option]
type NodeIndex = Int

data DLTable = DLTable { _names   :: [Item]
                       , _spacers :: [NodeIndex]
                       , _nodes   :: IntMap.IntMap Node
                       } deriving (Show)

data Node = Node { _topLen :: NodeIndex
                 , _llink  :: NodeIndex
                 , _rlink  :: NodeIndex
                 , _ulink  :: NodeIndex
                 , _dlink  :: NodeIndex
                 } deriving (Show)

makeLenses ''DLTable
makeLenses ''Node


----------------
-- Parsing -----
----------------

type Links = ([Item], [Option])

-- a
item :: Parser Item
item = some (satisfy isAlphaNum)

-- a d g
option :: Parser Option
option = (item <-> space) <* nl

-- a b c d e f g
--
-- c e
-- a d g
-- b c f
parseLinksFile :: Parser Links
parseLinksFile = (,) <$> (item <-> space) <* (nl >> nl)
                     <*> many option

nl    = char '\n'
(<->) = sepBy


------------------
-- Construction --
------------------

-- Build a DLTable from the items/options parsed from a links file
tableFromLinks :: Links -> DLTable
tableFromLinks (items, options) = built
  where
    len     = length items
    names   = sort items
    spacers = [len+1]

    root    = [ (0    , Node  0  len    1      0  0)                   ]
    tops    = [ (i    , Node  0  (i-1)  (i+1)  i  i) | i <- [1,2..len] ]
    spacer  = [ (len+1, Node  0  0      0      0  0)                   ]

    nodes   = root ++ tops ++ spacer

    -- Build the initial table containing nodes for the top row and the first spacer
    init    = DLTable names spacers (IntMap.fromList nodes)

    -- Fold all the options into the table one at a time
    built   = foldl addOption init options


-- Add an option of items to the DLTable, updating all links accordingly
addOption :: DLTable -> Option -> DLTable
addOption (DLTable names spacers nodes) items = DLTable names spacers' nodes'
  where
    len       = length items
    last      = head spacers
    spacerId  = last + len  + 1
    spacers'  = spacerId : spacers

    pairs     = zip [last+1, last+2 ..] (indicesOf names (sort items))

    -- Updated top nodes for each item in this option. The ulink will change to be the new
    -- node. If there is no existing dlink'd item it will be the new node, otherwise
    -- it is not changed
    tops'     = [ (i, Node (len' i +1) (i-1) (i+1) p          (newdn i p)) | (p, i) <- pairs ]

    -- The new item-level nodes introduced by this option
    new       = [ (p, Node i           (p-1) (p+1) (ulink' i) i          ) | (p, i) <- pairs ]

    -- Updated dlinks for the bottom item nodes
    bots'     = [ (b, Node i           (b-1) (b+1) (ulink' b) p          ) | (p, i) <- pairs,
                                                                              ulink' i /= i,
                                                                              let b = ulink' i ]

    newspacer = [ (spacerId, Node
                               (length spacers * (-1))
                               0
                               0
                               (last + 1)
                               0) ]

    -- Update the last spacer's dlink to point to the last of the newly added items
    spacer'   = [ (last, Node
                           (len' last)
                           0
                           0
                           (ulink' last)
                           (last + len )) ]

    updates   = tops' ++ new ++ bots' ++ newspacer ++ spacer'

    -- Fold the new updates into the table's node map
    nodes'    = foldr
                  (uncurry IntMap.insert)
                  nodes
                  updates

    ulink' n  = nodes IntMap.! n ^. ulink
    dlink' n  = nodes IntMap.! n ^. dlink
    len'   n  = nodes IntMap.! n ^. topLen

    newdn i p = if dlink' i == i
                then p
                else dlink' i


-- indicesOf "abcdefg" "ce" -> [3,5]
indicesOf :: Ord a => [a] -> [a] -> [Int]
indicesOf alphabet as = let
                          z   = zip alphabet [1..]
                          f a = case lookup a z of
                                   Nothing -> 0 -- really an error, we don't recognize this item
                                   Just x  -> x
                        in map f as


------------------
-- Operations ----
------------------

-- This is (12) from [Knuth].  Cover an item by unlinking its top node and all other items in
-- any option containing that item
cover :: NodeIndex -> DLTable -> DLTable
cover i (DLTable ns ss nodes) = DLTable ns ss nodes'
  where
    nodes' = go (node i ^. dlink) nodes
    node n = nodes IntMap.! n

    go p ns | p == i    = let l = node i ^. llink
                              r = node i ^. rlink
                          in  unlink l r ns
            | otherwise = go (node p ^. dlink) (hide p ns)

    -- Unlink the top node by rewiring the nodes on its left/right to each other
    unlink l r ns = let lnode = ns IntMap.! l
                        rnode = ns IntMap.! r
                    in  IntMap.insert l (lnode & rlink .~ r) $
                        IntMap.insert r (rnode & llink .~ l) ns


-- This is (13) from [Knuth]
hide :: NodeIndex -> IntMap.IntMap Node -> IntMap.IntMap Node
hide p nodes = nodes'
  where
    nodes' = go (p+1) nodes
    node n = nodes IntMap.! n

    go q ns | q == p    = ns
            | otherwise = let x = node q ^. topLen
                              u = node q ^. ulink
                              d = node q ^. dlink
                          in
                              -- Is q a spacer node
                              if x <= 0
                              then go u     ns
                              else go (q+1) (unlink x u d ns)

    -- Unlink a node by rewiring the nodes above/below it to each other
    unlink x u d ns = let unode = ns IntMap.! u
                          dnode = ns IntMap.! d
                          tnode = ns IntMap.! x
                      in
                          IntMap.insert x (tnode & topLen %~ subtract 1) $
                          IntMap.insert u (unode & dlink  .~ d         ) $
                          IntMap.insert d (dnode & ulink  .~ u         ) ns


-- This is (14) from [Knuth], the inverse operation to (12)
uncover :: NodeIndex -> DLTable -> DLTable
uncover i (DLTable ns ss nodes) = DLTable ns ss nodes'
  where
    nodes' = go (node i ^. ulink) nodes
    node n = nodes IntMap.! n

    go p ns | p == i    = let l = node i ^. llink
                              r = node i ^. rlink
                          in  relink l r ns
            | otherwise = go (node p ^. ulink) (unhide p ns)

    -- Relink the top node by rewiring the nodes on its left/right back to it
    relink l r ns = let lnode = ns IntMap.! l
                        rnode = ns IntMap.! r
                    in
                        IntMap.insert l (lnode & rlink .~ i) $
                        IntMap.insert r (rnode & llink .~ i) ns


-- This is (15) from [Knuth], the inverse operation to (13)
unhide :: NodeIndex -> IntMap.IntMap Node -> IntMap.IntMap Node
unhide p nodes = nodes'
  where
    nodes' = go (p-1) nodes
    node n = nodes IntMap.! n

    go q ns | q == p    = ns
            | otherwise = let x = node q ^. topLen
                              u = node q ^. ulink
                              d = node q ^. dlink
                          in
                              -- Is q a spacer node
                              if x <= 0
                              then go d     ns
                              else go (q-1) (relink x u d q ns)

    -- Relink a node by rewiring the nodes above/below it back to it
    relink x u d q ns = let unode = ns IntMap.! u
                            dnode = ns IntMap.! d
                            tnode = ns IntMap.! x
                        in
                            IntMap.insert x (tnode & topLen %~ (+1)) $
                            IntMap.insert u (unode & dlink  .~ q   ) $
                            IntMap.insert d (dnode & ulink  .~ q   ) ns


