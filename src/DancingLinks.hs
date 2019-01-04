{- DancingLinks.hs

   This is a Haskell implementation of Donald Knuth's Dancing Links. A draft of his upcoming
   installment of The Art of Computer Programming containing descriptions of the Dancing
   Links structure and algorithms is online (see references). 

   With this revision, we add the notion of secondary items. Solutions to the exact cover
   algorithm (Algorithm D) must now consist of options containing at least one primary
   item and at most one secondary item. The structure of the links file now contains
   separate lines to list primary and secondary items. All items are loaded into the node
   table, but when the algorithm selects the next item to cover, it disregards secondary
   items and considers only the primary ones.


   References:

   Donald Knuth introducing Dancing Links
     https://www.youtube.com/watch?v=_cR9zDlvP88

   [Knuth] The Art of Computer Programming, Vol 4, Pre-fascicle 5C
     https://www-cs-faculty.stanford.edu/~knuth/
-}

{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}

module DancingLinks
    ( tableFromLinks,
      linksTableFromFile,
      writeLinksFile,
      cover,
      uncover,
      algorithmD,
      AlgoState(..),
      DLTable(..),
      Item(..),
      Option(..),
      Solution(..)
    ) where

import           Control.Applicative (some, many, (<|>))
import           Control.Category    ((>>>))
import           Control.Lens
import           Control.Monad       (liftM)
import           Control.Monad.State
import           Data.Char           (isAlphaNum)
import           Data.List           (elemIndex, intercalate, nub, sort, sortBy)
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (comparing)

import qualified Data.IntMap as IntMap
import           NanoParsec hiding (item, option)


----------------
-- Types -------
----------------

type Item      = String
type Option    = [Item]
type Solution  = [Option]
type NodeIndex = Int
type NodeMap   = IntMap.IntMap Node

data DLTable = DLTable { _names          :: [Item]
                       , _primaryCount   :: Int
                       , _spacers        :: [NodeIndex]
                       , _nodes          :: NodeMap

                       -- Map nodes back to the options they belong to
                       , _options        :: IntMap.IntMap Option
                       , _nodesToOptions :: IntMap.IntMap Int
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

-- Primary items, secondary items, options
type Links = ([Item], [Item], [Option])

-- a
item :: Parser Item
item = some (satisfy isAlphaNum <|> char '-')

-- a d g
option :: Parser Option
option = (item <-> space) <* nl

-- pri: a b c d e f g
-- sec: x y z
--
-- c e y
-- a d g x
-- b c f z
parseLinksFile :: Parser Links
parseLinksFile = (,,) <$> itemList "pri: " <* nl
                      <*> itemList "sec: " <* (nl >> nl)
                      <*> many option
  where
    itemList :: String -> Parser [Item]
    itemList prefix = string prefix >> (item <-> space)

-- a b c d e f g
--
-- c e
-- a d g
-- b c f
--
-- Parse the prior links file version by considering all items primary and passing
-- an empty list for secondary options
parseOldLinksFile :: Parser Links
parseOldLinksFile = (,[],) <$> (item <-> space) <* (nl >> nl)
                           <*> many option

nl    = char '\n'
(<->) = sepBy

linksTableFromFile :: FilePath -> IO DLTable
linksTableFromFile file = readFile file >>= (run (parseLinksFile <|> parseOldLinksFile)
                                               >>> tableFromLinks
                                               >>> return)

writeLinksFile :: String -> ([Item], [Item], [Option]) -> IO ()
writeLinksFile name (primaries, secondaries, options) = writeFile path string
  where
    path = "inputs/" ++ name ++ ".links"

    string = unlines [ "pri: " ++ unwords primaries,
                       "sec: " ++ unwords secondaries,
                       "",
                       intercalate "\n" $ map (intercalate " ") options ]

------------------
-- Construction --
------------------

-- check :: Links -> Bool -- ([String])
-- check: Every option must include at least one primary item
-- check: Primary and secondary items distinct from each other
-- check: No options contain an item that is not a known item

-- Build a DLTable from the items/options parsed from a links file.
tableFromLinks :: Links -> DLTable
tableFromLinks (primaries, secondaries, options) = built
  where
    items   = primaries ++ secondaries
    len     = length items
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
    init    = DLTable items (length primaries) spacers (IntMap.fromList nodes) IntMap.empty IntMap.empty

    -- Fold all the options into the table one at a time
    built   = foldl addOption init options


-- Add an option of items to the DLTable, updating all links accordingly
addOption :: DLTable -> Option -> DLTable
addOption (DLTable names pc spacers nodes optionss optMap) items = DLTable names pc spacers' nodes' options' optMap'
  where
    len       = length items
    last      = head spacers
    spacerId  = last + len + 1
    spacers'  = spacerId : spacers

    pairs     = zip [last+1, last+2 ..] aligned

    -- Sort the items in this option in the order of the items as listed at the top of the links file
    aligned   = indicesIn names $ sortBy (comparing $ indexIn names) items

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

    -- Keep track of which option added these items
    optionId  = IntMap.size optionss + 1
    options'  = IntMap.insert optionId items optionss
    optMaps   = [ (p, optionId) | (p, i) <- pairs ]
    optMap'   = foldr
                  (uncurry IntMap.insert)
                  optMap
                  optMaps

    ulink' n  = (nodes IntMap.! n) ^. ulink
    dlink' n  = (nodes IntMap.! n) ^. dlink
    len'   n  = (nodes IntMap.! n) ^. topLen

    newdn i p = if dlink' i == i
                then p
                else dlink' i


-- indicesIn "abcdefg" "ce" -> [3,5]
indicesIn :: Ord a => [a] -> [a] -> [Int]
indicesIn alphabet as = let
                          z   = zip alphabet [1..]
                          f a = case lookup a z of
                                   Nothing -> 0 -- really an error, we don't recognize this item
                                   Just x  -> x
                        in map f as

-- indexIn "abcdefg" "c" -> 3
indexIn :: Ord a => [a] -> a -> Int
indexIn as a = fromMaybe 0 $ elemIndex a as

------------------
-- Operations ----
------------------

-- This is (12) from [Knuth].  Cover an item by unlinking its top node and all nodes in
-- any option containing that item (other than the nodes for the item being covered)
cover :: NodeIndex -> DLTable -> DLTable
cover i table = table & nodes .~ nodes'
  where
    ns     = table ^. nodes
    nodes' = go (ns IntMap.! i ^. dlink) ns

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
uncover i table = table & nodes .~ nodes'
  where
    ns     = table ^. nodes
    nodes' = go (ns IntMap.! i ^. ulink) ns

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
algorithmD table = evalState initialize start
  where
    start = AlgoState table IntMap.empty 0 0 []


{- The steps D1-D8 of Algorithm D, specified on p. 5 of [Knuth] -}

-- D1 - Initialize
initialize :: State AlgoState AlgoState
initialize = do
  state <- get
  put (state & level .~ 0)
  enterLevel


-- D2 - Enter level l
enterLevel :: State AlgoState AlgoState
enterLevel = do
  state <- get
  let nextI = chooseItem (state ^. table)

  if nextI == (-1) then addSolution >> leaveLevel
                   else chooseI

-- Visit a solution (ie, add it to the list of solutions we've found)
addSolution :: State AlgoState ()
addSolution = do
  state <- get
  level <- l

  let solution = [ option | i <- [0,1 .. level-1],
                            let xi       = (state ^. stack)                   IntMap.! i
                                optionId = (state ^. table ^. nodesToOptions) IntMap.! xi
                                option   = (state ^. table ^. options)        IntMap.! optionId ]

  put (state & solutions %~ (solution:))


-- D3 - Choose i
chooseI :: State AlgoState AlgoState
chooseI = do
  state <- get
  let chosen = chooseItem (state ^. table)

  put (state & i .~ chosen)
  coverI

-- Use the MRV heuristic (see exercise 9 in [Knuth]) to select the active item with the fewest
-- still-active options
chooseItem :: DLTable -> NodeIndex
chooseItem table
  | null byCount = -1
  | otherwise    = fst (head byCount)
  where
    byCount    = sortBy (comparing snd) candidates
    candidates = actives (table ^. nodes) (table ^. primaryCount)

    -- Get a list of still-active item indices and the number of active options containing the item
    actives :: NodeMap -> Int -> [(NodeIndex, Int)]
    actives nodes primaries = active
      where
        active = filter ((<=primaries) . fst) list
        list   = go (nodes IntMap.! 0 ^. rlink) []

        go i list | i == 0    = reverse list
                  | otherwise = let list' = (i, nodes IntMap.! i ^. topLen) : list
                                in  go (nodes IntMap.! i ^. rlink) list'


-- D4 - Cover i
coverI :: State AlgoState AlgoState
coverI = do
  i     <- geti
  dl    <- dl i

  coverS i
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
  i     <- geti
  xl    <- xl

  if i == xl then backtrack
             else loop (xl+1) xl

  -- Cover all j /= i in the option containing xl
  where loop p xl
          | p == xl   = adjustLevel 1 >> enterLevel
          | otherwise = do j <- topOf p
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
          | p /= xl   = do i  <- topOf p
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
backtrack = geti >>= uncoverS >> leaveLevel


-- D8 - Leave level l
leaveLevel :: State AlgoState AlgoState
leaveLevel = do
  level <- l

  if level == 0 then get
                else adjustLevel (-1) >> tryAgain


-- State getters
l :: State AlgoState Int
l = liftM (view level) get

geti :: State AlgoState Int
geti = liftM (view i) get

xl :: State AlgoState Int
xl = do
  state <- get
  level <- l
  return $ (state ^. stack) IntMap.! level

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

