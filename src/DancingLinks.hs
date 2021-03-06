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
      showItem,
      AlgoState(..),
      Option(..),
      Item(..),
      Links(..)
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

type Item      = (String, Coloring)
type Coloring  = String
type Option    = [Item]
type Solution  = [Option]
type Color     = Int
type NodeIndex = Int
type NodeMap   = IntMap.IntMap Node

data DLTable = DLTable { _names          :: [String]
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
                 , _color  :: Color
                 } deriving (Eq, Show)

makeLenses ''DLTable
makeLenses ''Node


----------------
-- Parsing -----
----------------

-- Primary items, secondary items, options
type Links = ([Item], [Item], [Option])

-- a
-- a:X
item :: Parser Item
item = (,) <$> some (satisfy isAlphaNum <|> char '-')
           <*> coloring

coloring :: Parser Coloring
coloring = (char ':' >> some (satisfy isAlphaNum))
           <|> return ""

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
    itemList prefix = string prefix >> ((item <-> space) <|> return [])

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

    string = unlines [ "pri: " ++ unwords (map fst primaries),
                       "sec: " ++ unwords (map fst secondaries),
                       "",
                       intercalate "\n" $ map (unwords . map showItem) options ]

showItem :: Item -> String
showItem (item, coloring)
  | coloring == "" = item
  | otherwise      = item ++ ":" ++ coloring

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
    names   = map fst items
    len     = length items
    spacers = [len+1]

    --                  Node topLen left  right up dn color
    ----------------------------------------------------------------------------
    root    = [ (0    , Node na     len   1     na na na )                     ]
    tops    = [ (i    , Node 0      (i-1) (i+1) i  i  na ) | i <- [1,2..len-1] ] ++
              [ (i    , Node 0      (i-1) 0     i  i  na ) | i <- [len]        ]
    spacer  = [ (len+1, Node 0      na    na    0  0  na )                     ]

    -- Some fields are not applicable for certain node types, so be explicit about this
    -- until we've refactored Node into multiple node types
    na      = 0

    -- Build the initial table containing nodes for the top row and the first spacer
    nodes   = root ++ tops ++ spacer
    init    = DLTable names (length primaries) spacers (IntMap.fromList nodes) IntMap.empty IntMap.empty

    -- Gather the known colorings from the options and pass it for indexing so addOption
    -- doesn't need to derive it each time
    colors  = options & concatMap (map snd) & nub & sort

    -- Fold all the options into the table one at a time
    built   = foldl (addOption colors) init options


-- Add an option of items to the DLTable, updating all links accordingly
addOption :: [Coloring] -> DLTable -> Option -> DLTable
addOption colors (DLTable names pc spacers nodes optionss optMap) items = DLTable names pc spacers' nodes' options' optMap'
  where
    len       = length items
    last      = head spacers
    spacerId  = last + len + 1
    spacers'  = spacerId : spacers

    pairs     = zip [last+1, last+2 ..] $ zip aligned colored

    -- Sort the items in this option in the order of the items as listed at the top of the links file
    aligned   = indicesIn names  1 $ map fst $ sortBy (comparing $ indexIn names . fst) items
    colored   = indicesIn colors 0 $ map snd $ sortBy (comparing $ indexIn names . fst) items

    -- Updated top nodes for each item in this option. The ulink will change to be the new
    -- node. If there is no existing dlink'd item it will be the new node, otherwise
    -- it is not changed. Remember to set the rlink of the right-most item to 0
    tops'     = [ (i, Node (len' i +1) (i-1) (i+1) p          (newdn i p) na ) | (p, (i, _)) <- pairs, i <  length names] ++
                [ (i, Node (len' i +1) (i-1) 0     p          (newdn i p) na ) | (p, (i, _)) <- pairs, i == length names]

    -- The new item-level nodes introduced by this option
    new       = [ (p, Node i           na    na    (ulink' i) i           c  ) | (p, (i, c)) <- pairs ]

    -- Updated dlinks for the bottom item nodes
    bots'     = [ (b, Node i           na    na    (ulink' b) p     (clr' b) ) | (p, (i, _)) <- pairs,
                                                                                  ulink' i /= i,
                                                                                  let b = ulink' i ]

    na        = 0
    newspacer = [ (spacerId, Node
                               (length spacers * (-1))
                               na
                               na
                               (last + 1)
                               0
                               na) ]

    -- Update the last spacer's dlink to point to the last of the newly added items
    spacer'   = [ (last, Node
                           (len' last)
                           na
                           na
                           (ulink' last)
                           (last + len)
                           na) ]

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
    clr'   n  = (nodes IntMap.! n) ^. color

    newdn i p = if dlink' i == i
                then p
                else dlink' i


-- indicesIn "abcdefg" 1 "ce" -> [3,5]
indicesIn :: Ord a => [a] -> Int -> [a] -> [Int]
indicesIn alphabet n as = let
                            z   = zip alphabet [n..]
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

-- This is (50) from [Knuth].  Cover an item by unlinking its top node and all nodes in
-- any option containing that item (other than the nodes for the item being covered)
cover :: NodeIndex -> DLTable -> DLTable
cover i table = table & nodes .~ nodes'
  where
    ns     = table ^. nodes
    nodes' = go ((^^.) ns i dlink) ns

    go p ns | p == i    = let l = (^^.) ns i llink
                              r = (^^.) ns i rlink
                          in  unlink l r ns
            | otherwise = go ((^^.) ns p dlink) (hide p ns)

    -- Unlink the top node by rewiring the nodes on its left/right to each other
    unlink l r ns = setLLink r l $
                    setRLink l r ns


-- This is (51) from [Knuth]
hide :: NodeIndex -> NodeMap -> NodeMap
hide p nodes = nodes'
  where
    nodes' = go (p+1) nodes

    go q ns | q == p    = ns
            | otherwise = let x = (^^.) ns q topLen
                              u = (^^.) ns q ulink
                              d = (^^.) ns q dlink
                          in
                              -- Is q a spacer node
                              if x <= 0 then go u ns
                                        else
                                             -- Skip this q if its color is < 0
                                             if (^^.) ns q color < 0
                                                then go (q+1) ns
                                                else go (q+1) (unlink x u d ns)

    -- Unlink a node by rewiring the nodes above/below it to each other
    -- Don't forget to decrement the number of active options for this item
    unlink x u d ns = setDLink u d $
                      setULink d u $
                      decLength x ns


-- This is (52) from [Knuth], the inverse operation to (50)
uncover :: NodeIndex -> DLTable -> DLTable
uncover i table = table & nodes .~ nodes'
  where
    ns     = table ^. nodes
    nodes' = go ((^^.) ns i ulink) ns

    go p ns | p == i    = let l = (^^.) ns i llink
                              r = (^^.) ns i rlink
                          in  relink l r i ns
            | otherwise = go ((^^.) ns p ulink) (unhide p ns)

    -- Relink the top node by rewiring the nodes on its left/right back to it
    relink l r i ns = setLLink r i $
                      setRLink l i ns


-- This is (53) from [Knuth], the inverse operation to (51)
unhide :: NodeIndex -> NodeMap -> NodeMap
unhide p nodes = nodes'
  where
    nodes' = go (p-1) nodes

    go q ns | q == p    = ns
            | otherwise = let x = (^^.) ns q topLen
                              u = (^^.) ns q ulink
                              d = (^^.) ns q dlink
                          in
                              -- Is q a spacer node
                              if x <= 0 then go d     ns
                                        else
                                             -- Skip this q if its color is < 0
                                             if (^^.) ns q color < 0
                                                then go (q-1) ns
                                                else go (q-1) (relink x u d q ns)

    -- Relink a node by rewiring the nodes above/below it back to it
    relink x u d q ns = setDLink u q $
                        setULink d q $
                        incLength x ns

-- Save some punctuation, by inventing new punctuation
(^^.) nodes p lens = nodes IntMap.! p ^. lens

-- Setter functions for node links
setLLink, setRLink, setDLink, setULink :: NodeIndex -> NodeIndex -> NodeMap -> NodeMap
setLLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & llink .~ tgt) nodes
setRLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & rlink .~ tgt) nodes
setDLink p tgt nodes = IntMap.insert p (nodes IntMap.! p & dlink .~ tgt) nodes
setULink p tgt nodes = IntMap.insert p (nodes IntMap.! p & ulink .~ tgt) nodes

setColor :: NodeIndex -> Color -> NodeMap -> NodeMap
setColor p c nodes = IntMap.insert p (nodes IntMap.! p & color .~ c) nodes

-- Increment/decrement the length field for a top node
decLength, incLength :: NodeIndex -> NodeMap -> NodeMap
decLength i nodes = IntMap.insert i (nodes IntMap.! i & topLen %~ subtract 1) nodes
incLength i nodes = IntMap.insert i (nodes IntMap.! i & topLen %~ (+1)      ) nodes


------------------
-- Algorithm C ---
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


{- The steps C1-C8 of Algorithm C, specified on p. 26 of [Knuth] -}

-- C1 - Initialize
initialize :: State AlgoState AlgoState
initialize = do
  state <- get
  put (state & level .~ 0)
  enterLevel


-- C2 - Enter level l
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


-- C3 - Choose i
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


-- C4 - Cover i
coverI :: State AlgoState AlgoState
coverI = do
  i     <- geti
  dl    <- prop dlink i

  coverS i
  updateStack dl
  tryXl

-- coverI above is the step D4 in the algorithm. coverS here is a helper function
-- to cover item i in the DLTable and persist it to the state
coverS :: NodeIndex -> State AlgoState ()
coverS i = do
  state <- get
  put (state & table %~ cover i)


-- C5 - Try x[l]
tryXl :: State AlgoState AlgoState
tryXl = do
  i     <- geti
  xl    <- xl

  if i == xl then backtrack
             else loop (xl+1) xl

  -- Cover all j /= i in the option containing xl
  where loop p xl
          | p == xl   = adjustLevel 1 >> enterLevel
          | otherwise = do j <- prop topLen p
                           u <- prop ulink p

                           if j <= 0 then loop u xl
                                     else commit p j >> loop (p+1) xl

adjustLevel :: Int -> State AlgoState ()
adjustLevel delta = do
  state <- get
  put (state & level %~ (+delta))

-- This is (54) from [Knuth]
commit :: NodeIndex -> NodeIndex -> State AlgoState ()
commit p j = do
  c <- prop color p

  if c == 0 then coverS j
            else when (c > 0) (purify p)

-- This is (56) from [Knuth]
uncommit :: NodeIndex -> NodeIndex -> State AlgoState ()
uncommit p j = do
  c <- prop color p

  if c == 0 then uncoverS j
            else when (c > 0) (unpurify p)

-- This is (55) from [Knuth]
purify :: NodeIndex -> State AlgoState ()
purify p = do
  c <- prop color p
  i <- prop topLen p
  q <- prop dlink i

  go q c i

  where go :: NodeIndex -> Color -> Int -> State AlgoState ()
        go q c i
          | q == i    = return ()
          | otherwise = do c' <- prop color q
                           q' <- prop dlink q

                           if c' /= c then hideS q >> go q' c i
                                      else if q /= p
                                           then setColorS q (-1) >> go q' c i
                                           else                     go q' c i

-- This is (57) from [Knuth]
unpurify :: NodeIndex -> State AlgoState ()
unpurify p = do
  c <- prop color p
  i <- prop topLen p
  q <- prop ulink i

  go q c i

  where go :: NodeIndex -> Color -> Int -> State AlgoState ()
        go q c i
          | q == i    = return ()
          | otherwise = do c' <- prop color q
                           q' <- prop ulink q

                           if c' < 0  then setColorS q c >> go q' c i
                                      else if q /= p
                                           then unhideS q >> go q' c i
                                           else              go q' c i

hideS :: NodeIndex -> State AlgoState ()
hideS p = do
  state <- get
  put $ state & table . nodes %~ hide p

unhideS :: NodeIndex -> State AlgoState ()
unhideS p = do
  state <- get
  put $ state & table . nodes %~ unhide p

setColorS :: NodeIndex -> Color -> State AlgoState ()
setColorS p c = do
  state <- get
  put $ state & table . nodes %~ setColor p c


-- C6 - Try again
tryAgain :: State AlgoState AlgoState
tryAgain = do
  level <- l
  xl    <- xl

  loop (xl-1) xl

  -- Uncover all j /= i in the option containing xl
  where loop p xl
          | p /= xl   = do j  <- prop topLen p
                           dl <- prop dlink p

                           if j <= 0 then loop dl xl
                                     else uncommit p j >> loop (p-1) xl

          | otherwise = do i  <- prop topLen xl
                           xl <- prop dlink xl

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


-- C7 - Backtrack
backtrack :: State AlgoState AlgoState
backtrack = geti >>= uncoverS >> leaveLevel


-- C8 - Leave level l
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

prop :: Getting a Node a -> NodeIndex -> State AlgoState a
prop lens p = do
  state <- get
  return $ (state ^. table ^. nodes) IntMap.! p ^. lens

