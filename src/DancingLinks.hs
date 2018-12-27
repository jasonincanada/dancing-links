{- DancingLinks.hs

   This is a Haskell implementation of Donald Knuth's Dancing Links. A draft of his upcoming
   installment of The Art of Computer Programming containing descriptions of the Dancing
   Links structure and algorithms is online (see references). 

   The code implemented below and executed with the inputs/table10.links file parses, loads,
   and exactly reproduces table (10) in Knuth (p. 3). Calling "main" will print out the
   values in Table 1 on (p. 4).

   So it is not too useful yet but this is the initial version that loads in and understands
   the format of a *.links file.


   References:

   Donald Knuth introducing Dancing Links
     https://www.youtube.com/watch?v=_cR9zDlvP88

   The Art of Computer Programming, Vol 4, Pre-fascicle 5C
     https://www-cs-faculty.stanford.edu/~knuth/
-}

{-# Language TemplateHaskell #-}

module DancingLinks
    ( parseLinksFile,
      tableFromLinks
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

data Node = Node { _index  :: NodeIndex
                 , _topLen :: NodeIndex
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
item :: Parser String
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

    root    = [ (0    , Node 0       0 len   1     0 0)                   ]
    tops    = [ (i    , Node i       0 (i-1) (i+1) i i) | i <- [1,2..len] ]
    spacer  = [ (len+1, Node (len+1) 0 0     0     0 0)                   ]

    nodes   = root ++ tops ++ spacer

    -- Build the initial table containing nodes for the top row and the first spacer
    init    = DLTable names spacers (IntMap.fromList nodes)

    -- Fold all the options into the table one at a time
    built   = foldl addOption init options


-- Add an option of items to the DLTable, updating all links accordingly
addOption :: DLTable -> Option -> DLTable
addOption (DLTable names spacers nodes) items = DLTable names spacers' nodes'
  where
    size      = length items
    last      = head spacers
    spacerId  = last + size + 1
    spacers'  = spacerId : spacers

    pairs     = zip [last+1, last+2 ..] (indicesOf names (sort items))

    -- Updated top nodes for each item in this option. The ulink will change to be the new
    -- node. If there is no existing dlink'd item it will be the new node, otherwise
    -- it is not changed
    tops'     = [ (i, Node i (len' i +1) (i-1) (i+1) p          (newdn i p)) | (p, i) <- pairs ]

    -- The new item-level nodes introduced by this option
    new       = [ (p, Node p i           (p-1) (p+1) (ulink' i) i          ) | (p, i) <- pairs ]

    -- Updated dlinks for the bottom item nodes
    bots'     = [ (b, Node b i           (b-1) (b+1) (ulink' b) p          ) | (p, i) <- pairs,
                                                                                ulink' i /= i,
                                                                                let b = ulink' i ]

    newspacer = [ (spacerId, Node
                               spacerId
                               (length spacers * (-1))
                               0
                               0
                               (last + 1)
                               0) ]

    -- Update the last spacer's dlink to point to the last of the newly added items
    spacer'   = [ (last, Node
                           last
                           (len' last)
                           0
                           0
                           (ulink' last)
                           (last + size)) ]

    updates   = tops' ++ new ++ bots' ++ newspacer ++ spacer'

    -- Fold the new updates into the table's node map
    nodes'    = foldr
                  (\(i, node) -> IntMap.insert i node)
                  nodes
                  updates

    ulink' n  = nodes IntMap.! n ^. ulink
    dlink' n  = nodes IntMap.! n ^. dlink
    len'   n  = nodes IntMap.! n ^. topLen

    newdn i p = if dlink' i == i
                then p
                else dlink' i

    newup i p = if ulink' i == i
                then p
                else ulink' i


-- indicesOf "abcdefg" "ce" -> [3,5]
indicesOf :: Ord a => [a] -> [a] -> [Int]
indicesOf alphabet as = let
                          z   = zip alphabet [1..]
                          f a = case lookup a z of
                                   Nothing -> 0 -- really an error, we don't recognize this item
                                   Just x  -> x
                        in map f as

