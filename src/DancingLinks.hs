{- DancingLinks.hs

   This is a Haskell implementation of Donald Knuth's Dancing Links. A draft of his upcoming
   installment of The Art of Computer Programming containing descriptions of the Dancing
   Links structure and algorithms is online (see references). 

   The code implemented below and executed with the inputs/table10.links file parses, loads,
   and exactly reproduces table (10) in Knuth (p. 3). Calling "main" will print out the
   values in Table 1 on (p. 4).

   So it is not too useful yet but this is the initial version that loads in and understands
   the format of a *.links file.

   This revision implements Algorithm D to find all possible solutions to the exact cover
   problem on a dynamic links table


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
      uncover,
      algorithmD,
      DLTable(..),
      AlgoState(..)
    ) where

import           Control.Applicative (some, many)
import           Control.Lens
import           Control.Monad       (liftM)
import           Control.Monad.State
import           Data.Char           (isAlphaNum)
import           Data.List           (sort, sortBy)
import           Data.Ord            (comparing)

import qualified Data.IntMap as IntMap
import           NanoParsec hiding (item, option)


----------------
-- Types -------
----------------

type Item      = String
type Option    = [Item]
type Solution  = [NodeIndex]
type NodeIndex = Int
type NodeMap   = IntMap.IntMap Node

data DLTable = DLTable { _names   :: [Item]
                       , _spacers :: [NodeIndex]
                       , _nodes   :: NodeMap
                       } deriving (Eq, Show)

data Node = Node { _topLen :: NodeIndex
                 , _llink  :: NodeIndex
                 , _rlink  :: NodeIndex
                 , _ulink  :: NodeIndex
                 , _dlink  :: NodeIndex
                 } deriving (Eq, Show)

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

    --                  Node topLen left  right up down
    ------------------------------------------------------------------------
    root    = [ (0    , Node na     len   1     na na)                     ]
    tops    = [ (i    , Node 0      (i-1) (i+1) i  i ) | i <- [1,2..len-1] ] ++
              [ (i    , Node 0      (i-1) 0     i  i ) | i <- [len]        ]
    spacer  = [ (len+1, Node 0      na    na    0  0 )                     ]

    -- Some fields are not applicable for certain node types, so be explicit about this
    -- until we've refactored Node into multiple node types
    na      = 0

    -- Build the initial table containing nodes for the top row and the first spacer
    nodes   = root ++ tops ++ spacer
    init    = DLTable names spacers (IntMap.fromList nodes)

    -- Fold all the options into the table one at a time
    built   = foldl addOption init options


-- Add an option of items to the DLTable, updating all links accordingly
addOption :: DLTable -> Option -> DLTable
addOption (DLTable names spacers nodes) items = DLTable names spacers' nodes'
  where
    len       = length items
    last      = head spacers
    spacerId  = last + len + 1
    spacers'  = spacerId : spacers

    pairs     = zip [last+1, last+2 ..] (indicesOf names (sort items))

    -- Updated top nodes for each item in this option. The ulink will change to be the new
    -- node. If there is no existing dlink'd item it will be the new node, otherwise
    -- it is not changed. Remember to set the rlink of the right-most item to 0
    tops'     = [ (i, Node (len' i +1) (i-1) (i+1) p          (newdn i p)) | (p, i) <- pairs, i <  length names] ++
                [ (i, Node (len' i +1) (i-1) 0     p          (newdn i p)) | (p, i) <- pairs, i == length names]

    -- The new item-level nodes introduced by this option
    new       = [ (p, Node i           na    na    (ulink' i) i          ) | (p, i) <- pairs ]

    -- Updated dlinks for the bottom item nodes
    bots'     = [ (b, Node i           na    na    (ulink' b) p          ) | (p, i) <- pairs,
                                                                              ulink' i /= i,
                                                                              let b = ulink' i ]

    na        = 0
    newspacer = [ (spacerId, Node
                               (length spacers * (-1))
                               na
                               na
                               (last + 1)
                               0) ]

    -- Update the last spacer's dlink to point to the last of the newly added items
    spacer'   = [ (last, Node
                           (len' last)
                           0
                           0
                           (ulink' last)
                           (last + len)) ]

    -- Fold the new updates into the table's node map
    updates   = tops' ++ new ++ bots' ++ newspacer ++ spacer'
    nodes'    = foldr
                  (uncurry IntMap.insert)
                  nodes
                  updates

    ulink' n  = (nodes IntMap.! n) ^. ulink
    dlink' n  = (nodes IntMap.! n) ^. dlink
    len'   n  = (nodes IntMap.! n) ^. topLen

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
    nodes' = go (nodes IntMap.! i ^. dlink) nodes

    go p ns | p == i    = let l = ns IntMap.! i ^. llink
                              r = ns IntMap.! i ^. rlink
                          in  unlink l r ns
            | otherwise = go (ns IntMap.! p ^. dlink) (hide p ns)

    -- Unlink the top node by rewiring the nodes on its left/right to each other
    unlink l r ns = setLLink r l $
                    setRLink l r ns


-- This is (13) from [Knuth]
hide :: NodeIndex -> NodeMap -> NodeMap
hide p nodes = nodes'
  where
    nodes' = go (p+1) nodes

    go q ns | q == p    = ns
            | otherwise = let x = ns IntMap.! q ^. topLen
                              u = ns IntMap.! q ^. ulink
                              d = ns IntMap.! q ^. dlink
                          in
                              -- Is q a spacer node
                              if x <= 0 then go u     ns
                                        else go (q+1) (unlink x u d ns)

    -- Unlink a node by rewiring the nodes above/below it to each other
    -- Don't forget to decrement the number of active options for this item
    unlink x u d ns = setDLink u d $
                      setULink d u $
                      decLength x ns


-- This is (14) from [Knuth], the inverse operation to (12)
uncover :: NodeIndex -> DLTable -> DLTable
uncover i (DLTable ns ss nodes) = DLTable ns ss nodes'
  where
    nodes' = go (nodes IntMap.! i ^. ulink) nodes

    go p ns | p == i    = let l = ns IntMap.! i ^. llink
                              r = ns IntMap.! i ^. rlink
                          in  relink l r i ns
            | otherwise = go (ns IntMap.! p ^. ulink) (unhide p ns)

    -- Relink the top node by rewiring the nodes on its left/right back to it
    relink l r i ns = setLLink r i $
                      setRLink l i ns


-- This is (15) from [Knuth], the inverse operation to (13)
unhide :: NodeIndex -> NodeMap -> NodeMap
unhide p nodes = nodes'
  where
    nodes' = go (p-1) nodes

    go q ns | q == p    = ns
            | otherwise = let x = ns IntMap.! q ^. topLen
                              u = ns IntMap.! q ^. ulink
                              d = ns IntMap.! q ^. dlink
                          in
                              -- Is q a spacer node
                              if x <= 0 then go d     ns
                                        else go (q-1) (relink x u d q ns)

    -- Relink a node by rewiring the nodes above/below it back to it
    relink x u d q ns = setDLink u q $
                        setULink d q $
                        incLength x ns

-- Setter functions for node links
setLLink, setRLink, setDLink, setULink :: NodeIndex -> NodeIndex -> NodeMap -> NodeMap
setLLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & llink .~ tgt) nodes
setRLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & rlink .~ tgt) nodes
setDLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & dlink .~ tgt) nodes
setULink p tgt nodes = IntMap.insert p (nodes IntMap.! p & ulink .~ tgt) nodes

-- Increment/decrement the length field for a top node
decLength, incLength :: NodeIndex -> NodeMap -> NodeMap
decLength i nodes = IntMap.insert i (nodes IntMap.! i & topLen %~ subtract 1) nodes
incLength i nodes = IntMap.insert i (nodes IntMap.! i & topLen %~ (+1)      ) nodes


------------------
-- Algorithm D ---
------------------

data AlgoState = AlgoState { _table     :: DLTable
                           , _stack     :: IntMap.IntMap Int
                           , _level     :: Int
                           , _i         :: Int
                           , _solutions :: [Solution]
                           } deriving (Show)

makeLenses ''AlgoState

-- Take a dynamic links table and run the exact cover algorithm on it,
-- returning the final state with all solutions
algorithmD :: DLTable -> AlgoState
algorithmD table = evalState enterLevel start
  where
    start = AlgoState table IntMap.empty 0 0 []


{- The steps D1-D8 of Algorithm D, specified on p. 5 of [Knuth] -}

-- D1 - Initialize
init :: State AlgoState AlgoState
init = do
  state <- get
  put (state & level .~ 0)
  enterLevel


-- D2 - Enter level l
enterLevel :: State AlgoState AlgoState
enterLevel = do
  rl <- rl 0

  if rl == 0 then addSolution >> leaveLevel
             else chooseI

-- This currently adds the individual nodes as the solution, really it should
-- add the whole option the nodes belong to
addSolution :: State AlgoState ()
addSolution = do
  state <- get
  level <- l

  let solution = [ (state ^. stack) IntMap.! i | i <- [0,1..level-1] ]

  put (state & solutions %~ (solution:))


-- D3 - Choose i
chooseI :: State AlgoState AlgoState
chooseI = do
  state <- get
  let chosen = randomi (state ^. table ^. nodes)

  put (state & i .~ chosen)
  coverI

  where
    -- This isn't yet random, it uses the MRV heuristic (see exercise 9 in [Knuth]) to select
    -- the item with the fewest still active options
    randomi :: NodeMap -> NodeIndex
    randomi nodes = let candidates = actives nodes
                        byCount    = sortBy (comparing snd) candidates
                        fewest     = head byCount
                    in  fst fewest

    -- Get a list of still-active item indices and the number of active options containing the item
    actives :: NodeMap -> [(NodeIndex, Int)]
    actives nodes = go (nodes IntMap.! 0 ^. rlink) []
      where
        go i list | i == 0    = list
                  | otherwise = let list' = (i, nodes IntMap.! i ^. topLen) : list
                                in  go (nodes IntMap.! i ^. rlink) list'


-- D4 - Cover i
coverI :: State AlgoState AlgoState
coverI = do
  state <- get

  let i' = state ^. i
      dl = (state ^. table ^. nodes) IntMap.! i' ^. dlink

  coverS i'
  updateStack dl
  tryXl

-- coverI above is the step D4 in the algorithm. coverS here is a helper function
-- to cover item i in the DLTable and persist it to the state
coverS :: NodeIndex -> State AlgoState ()
coverS i = do
  state <- get
  put (state & table %~ cover i)


-- D5 - Try x[l]
tryXl :: State AlgoState AlgoState
tryXl = do
  state <- get
  xl    <- xl

  if state ^. i == xl then backtrack
                      else loop (xl+1) xl

  -- Cover all j /= i in the option containing xl
  where loop p xl
          | p == xl   = adjustLevel 1 >> enterLevel
          | otherwise = do j <- topLenOf p
                           u <- ul p

                           if j <= 0 then loop u xl
                                     else coverS j >> loop (p+1) xl

adjustLevel :: Int -> State AlgoState ()
adjustLevel delta = do
  state <- get
  put (state & level %~ (+delta))


-- D6 - Try again
tryAgain :: State AlgoState AlgoState
tryAgain = do
  level <- l
  xl    <- xl

  loop (xl-1) xl

  -- Uncover all j /= i in the option containing xl
  where loop p xl
          | p /= xl   = do i  <- topLenOf p
                           dl <- dl p

                           if i <= 0 then loop dl xl
                                     else uncoverS i >> loop (p-1) xl

          | otherwise = do i  <- topOf xl
                           xl <- dl xl

                           updateStack xl
                           updateI i
                           tryXl

uncoverS :: NodeIndex -> State AlgoState ()
uncoverS i = do
  state <- get
  put (state & table %~ uncover i)

-- Update the stack at the current level l
updateStack :: NodeIndex -> State AlgoState ()
updateStack p = do
  state <- get
  level <- l
  put (state & stack %~ IntMap.insert level p)

updateI :: NodeIndex -> State AlgoState ()
updateI p = do
  state <- get
  put (state & i .~ p)


-- D7 - Backtrack
backtrack :: State AlgoState AlgoState
backtrack = do
  state <- get

  let tbl = state ^. table
      i'  = state ^. i

  put (state & table .~ uncover i' tbl)
  leaveLevel 


-- D8 - Leave level l
leaveLevel :: State AlgoState AlgoState
leaveLevel = do
  level <- l

  if level == 0 then get
                else adjustLevel (-1) >> tryAgain


-- State getters
l :: State AlgoState Int
l = liftM (view level) get

xl :: State AlgoState Int
xl = do
  state <- get
  level <- l
  return $ (state ^. stack) IntMap.! level

topLenOf :: NodeIndex -> State AlgoState Int
topLenOf p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. topLen

ll, rl, dl, ul :: NodeIndex -> State AlgoState Int
ll p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. llink

rl p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. rlink

dl p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. dlink

ul p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. ulink

topOf :: NodeIndex -> State AlgoState Int
topOf p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. topLen

