{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

import System.IO --(hSetBuffering, stdin, stdout, LineBuffering)
import qualified System.Random as R (split)
import System.Random (StdGen, RandomGen, newStdGen, randomR)
import Data.List (sort, unfoldr, maximumBy)
import Debug.Trace (trace)

-- import Control.Monad.Random (Rand, evalRand)
import Data.Maybe (fromJust)
import Data.Tree (Tree(..), Forest)
import Data.Ord (comparing)
-- import Data.Array.Diff
import Data.Array.Unboxed
import Text.Printf (printf)


-- import Data.Tree.Zipper (TreeLoc, tree, fromTree, hasChildren, setTree, parent, getChild)


type TronMap = UArray (Int, Int) Char
-- type TronMap = DiffUArray (Int, Int) Char

setBuffers :: IO ()
setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

main :: IO ()
main = newStdGen >>= playBot uctBot

                                     
playBot :: (GameState -> StdGen -> (Move, StdGen)) -> StdGen -> IO ()
playBot bot starting = do
    setBuffers
    interact (playTurns bot starting . lines)

readInt :: String -> Int
readInt = read

-- readSpot :: Char -> Spot
-- readSpot '#' = Wall
-- readSpot ' ' = Blank
-- readSpot '1' = Player
-- readSpot '2' = Enemy
-- readSpot _ = error "unsupported spot character"

showSpot :: Spot -> Char
showSpot Wall = '#'
showSpot Blank = ' '
showSpot Player = '1'
showSpot Enemy = '2'

makeMove :: Move -> String
makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns :: (GameState -> StdGen -> (Move, StdGen))
             -> StdGen
             -> [String]
             -> String
playTurns _bot _pastValue [] = ""
playTurns bot pastValue str =
    -- trace ("playTurns " ++ (show move))
    makeMove move ++ "\n" ++ playTurns bot history (drop (h+1) str)
    where [w,h] = map readInt (words $ head str)

          gameState =
              GameState { getTronMap = tronMap
                        , ourRandomGen = pastValue
                        , moveHistory = []
                        , playerCrashed = False
                        , enemyCrashed = False
                        , playerPos = initPos tronMap Player
                        , enemyPos = initPos tronMap Enemy
                        , maxX = w
                        , maxY = h
                        , toMove = Player
                        }
          tronMap =
              array
              ((1, 1), (w, h))
              $ zip [(x,y) | y <- [1 .. h], x <- [1 .. w]]
              -- $ map readSpot
              $ concat $ tail str

          -- tronMap = map (map readSpot) (take h (tail str))
	  (move, history) =
              -- trace ("playTurns\n" ++
              --        (unlines $ map (unwords . (map show)) tronMap))
              bot gameState pastValue



data Spot = Wall | Blank | Player | Enemy deriving Eq
data Move = North | East | South | West deriving (Eq, Show)


instance Show Spot where
    show Wall = "#"
    show Blank = " "
    show Player = "1"
    show Enemy = "2"

data GameState = GameState {
      getTronMap      :: TronMap
     ,moveHistory     :: [(Spot, Move)]
     ,ourRandomGen    :: StdGen
     ,playerCrashed   :: Bool
     ,enemyCrashed    :: Bool
     ,playerPos       :: (Int,Int)
     ,enemyPos        :: (Int,Int)
     ,maxX            :: Int
     ,maxY            :: Int
     ,toMove          :: Spot
    }


instance Show GameState where
    show state =
        case moveHistory state of
          [] -> ""
          moves ->
              show $ reverse moves

instance UctNode GameState where
    isTerminalNode state =
        -- trace ("isTerminalNode " ++
        --        (show (state, ("complete",complete), ("crashes",anyCrashes), result)))
        result
        where
          result = complete && anyCrashes
          complete = completeRound state
          anyCrashes =
              playerCrashed state || enemyCrashed state

    finalResult state =
        finalResult' state $ lastToMove state

    randomEvalOnce state =
      return $ runOneRandom state (ourRandomGen state)

    children state =
        -- trace ("children " ++
        --        (show (state, moves)))
        map (updateGameState state) moves
        where
          moves =
              case possibleMoves
                   tronMap
                   (moverPos state) of
                [] -> [North]
                ls -> ls
          tronMap = getTronMap state


finalResult' :: GameState -> Spot -> Float
finalResult' state who =
        -- trace ("finalResult2 "
        --        ++ show (((playerCrashed state), (enemyCrashed state)),who,result)
        --        ++ showMap (getTronMap state)
        --        -- show moveHistory state) ++
        --       )
        result
        where
          result = 
              case ((playerCrashed state), (enemyCrashed state)) of
                (True, True) -> 0.5
                (True, False) ->
                    case who of
                      Player -> 0.0
                      Enemy -> 1.0
                      _ -> error "finalResult' who set to non player param"
                (False, True) ->
                    case who of
                      Player -> 1.0
                      Enemy -> 0.0
                      _ -> error "finalResult' who set to non player param"
                (False, False) ->
                    error "finalResult' called when neither player crashed"



uctBot :: GameState -> StdGen -> (Move, StdGen)
uctBot state rGen =
    case possibleMoves tronMap (playerPos state) of
      [] ->
          trace "uctBot no possible moves, go North"
           (North, rGen)
      [onlyMove] ->
          trace ("uctBot only one move: " ++ show onlyMove)
           (onlyMove, rGen)
      _otherwise -> (move, g')
    where
      move =
          case pv of
            [] ->
                trace "uctBot uct found no moves, go North"
                North
            pv' ->
                trace ("uctBot\n"
                       ++ show alternateFirstMoves ++ "\n"
                       -- ++ show pv' ++ "\n"
                      )
                snd $ last $ moveHistory $ nodeState $
                    head pv'

      alternateFirstMoves =
          map rootLabel $ reverse $ sort $ subForest t

      pv = principalVariation t
      t = uct state 1000 g
      
      tronMap = getTronMap state
      (g, g') = R.split rGen



runOneRandom :: GameState -> StdGen -> Float
runOneRandom initState initGen =
    -- trace ("runOneRandom " ++ showMap (getTronMap initState))
    run initState initGen 0
    where
      run :: GameState -> StdGen -> Int -> Float
      run _ _ 1000 =
          trace "run returning after 1000 iterations"
          0.5
      run state rGen runCount =
          if isTerminalNode state'
          then
              -- trace ("run is terminal")
              finalResult' state' who
          else
              run state' rGen' (runCount + 1)
          where
            state' = updateGameState state move
            (move, rGen') = genMoveRand state rGen

      who = lastToMove initState

genMoveRand :: GameState -> StdGen -> (Move, StdGen)
genMoveRand state rGen =
    -- trace ("\n\ngenMoveRand" ++
    --        showMap tronMap ++
    --        show (who, move, moveList))
    (move, rGen')
    where
      (move, rGen') =
          case moveList of
            [] -> (North, rGen)
            [singleMove] -> (singleMove, rGen)
            moves -> pick moves rGen
      moveList = possibleMoves tronMap (moverPos state)
      tronMap = getTronMap state

pick :: (Show a) => [a] -> StdGen -> (a, StdGen)
pick as rGen =
    -- trace ("pick " ++ (show (as, i)))
    (as !! i, rGen')
    where
      (i, rGen') = randomR (0, (length as - 1)) rGen


updateGameState :: GameState -> Move -> GameState
updateGameState state move =
    -- trace ("\n\n" ++
    --        "updateGameState\n" ++
    --        show (moveHistory state) ++
    --        "\n" ++
    --        showMap tronMap ++
    --        show ("crashed",(playerCrashed state, enemyCrashed state)) ++
    --        "\n" ++
    --        show ("toMove ",toMove state) ++
    --        "\n" ++
    --        show (who, move) ++
    --        "\n" ++
    --        showMap tronMap' ++
    --        show ("crashed'", (playerCrashed', enemyCrashed')) ++
    --        "\n" ++
    --        show ("toMove ",toMove state') ++
    --        "\n")
    state'
    where
      state' =
          state { getTronMap = tronMap'
                , moveHistory = moveHistory'
                , playerCrashed = playerCrashed'
                , enemyCrashed = enemyCrashed'
                , playerPos = playerPos'
                , enemyPos = enemyPos'
                , toMove = lastToMove state
                }

      playerCrashed' =
          case who of
            Player ->
                crashed tronMap playerPos'
            Enemy ->
                playerCrashed state || bothCrashed
            _ -> error "playerCrashed' who set to non player param"
      enemyCrashed' =
          case who of
            Player ->
                False
            Enemy ->
                crashed tronMap enemyPos' || bothCrashed
            _ -> error "enemyCrashed' who set to non player param"

      bothCrashed = playerPos' == enemyPos'

      playerPos' =
          case who of
            Player -> updatePos (playerPos state) move
            Enemy -> (playerPos state)
            _ -> error "playerPos' who set to non player param"
      enemyPos' =
          case who of
            Player -> (enemyPos state)
            Enemy -> updatePos (enemyPos state) move
            _ -> error "enemyPos' who set to non player param"

      tronMap' = tronMap // updates
      updates = [(position, (showSpot Wall)),
                 (position',(showSpot who))]

      position =
          case who of
            Player -> (playerPos state)
            Enemy -> (enemyPos state)
            _ -> error "position who set to non player param"
      position' =
          case who of
            Player -> playerPos'
            Enemy -> enemyPos'
            _ -> error "position' who set to non player param"


      moveHistory' = moveHistory state ++ [(who, move)]
      tronMap = getTronMap state
      who = toMove state


initPos :: TronMap -> Spot -> (Int, Int)
initPos tronMap who =
    fst $ head $ filter ((== (showSpot who)) . snd) $ assocs tronMap

possibleMoves :: TronMap -> (Int, Int) -> [Move]
possibleMoves tronMap position =
    -- trace ("possibleMoves " ++ show position ++
    --        showMap tronMap ++ show moves)
    moves
    where
      moves =
          filter possibleMove [North, East, South, West]
      possibleMove move =
          (tronMap ! updatePos position move) /= showSpot Wall

updatePos :: (Int, Int) -> Move -> (Int, Int)
updatePos (x, y) North = (x, y-1)
updatePos (x, y) South = (x, y+1)
updatePos (x, y) East = (x+1, y)
updatePos (x, y) West = (x-1, y)

crashed :: TronMap -> (Int, Int) -> Bool
crashed tronMap p =
    -- trace ("crashed: " ++ show (p, result))
    result
    where
      result = (tronMap ! p) /= showSpot Blank

-- showMap :: TronMap -> String
-- showMap tronMap =
--     "\n" ++
--     show (assocs tronMap)
--     ++ "\n"

completeRound :: GameState -> Bool
completeRound state = Player == toMove state

moverPos :: GameState -> (Int, Int)
moverPos state =
    case toMove state of
      Player -> playerPos state
      Enemy -> enemyPos state
      _ -> error "moverPos used with unsupported param"

lastToMove :: GameState -> Spot
lastToMove state =
    case toMove state of
      Player -> Enemy
      Enemy -> Player
      _ -> error "lastToMove used with unsupported param"








class (Show a) => UctNode a where
    isTerminalNode :: a -> Bool
    finalResult :: a -> Float
    randomEvalOnce :: (RandomGen g) => a -> g -> Float
    children :: a -> [a]

instance (UctNode a) => Show (UctLabel a) where
    show label =
        show (nodeState label) ++ " " ++
        printf "%.2f " (winningProb label) ++
        show (visits label) ++ dStr ++ " - "
        where
          dStr = if isDone label
                 then "+ "
                 else ""

data UctLabel a = UctLabel {
     nodeState       :: a
    ,winningProb     :: Float
    ,visits          :: Int
    ,isDone          :: Bool
    }



instance Eq (UctLabel a) where
    (==) a b =
        (winningProb a == winningProb b)
        && (visits a == visits b)
        && (isDone a == isDone b)

instance Ord (Tree (UctLabel b)) where
    compare x y =
        case compare (f x) (f y) of
          EQ -> compare (g x) (g y)
          order -> order
        where
          f = winningProb . rootLabel
          g = visits . rootLabel

defaultUctLabel :: UctLabel a
defaultUctLabel = UctLabel {
                    nodeState = undefined
                  , winningProb = 0.5
                  -- , uctValue = 0.5
                  , visits = 1
                  , isDone = False
                  }

exploratoryC :: Float
exploratoryC = 1

uct :: (UctNode a, RandomGen g) => a -> Int -> g -> Tree (UctLabel a)
uct initState n rGen =
    tree (uctZipper (fromTree $ makeNodeWithChildren initState) n rGen)


uctZipper :: (UctNode a, RandomGen g) =>
             TreeLoc (UctLabel a) ->
             Int ->
             g ->
             TreeLoc (UctLabel a)
uctZipper loc 0 _rGen = loc
uctZipper loc n rGen =
    if done
    then loc'
    else uctZipper loc' (n-1) rGen''
    where
      (loc', done) = uctZipperDown loc rGen'
      (rGen', rGen'') = R.split rGen


uctZipperDown  :: (UctNode a, RandomGen g) => TreeLoc (UctLabel a) -> g -> ((TreeLoc (UctLabel a)), Bool)
uctZipperDown loc rGen
    | hasChildren loc =
        -- trace ("uctZipperDown hasChildren, recursing "
        --        ++ (show $ nodeState $ rootLabel $ tree uctMax))
        uctZipperDown uctMax rGen
    | isTerminalNode state =
        -- trace ("uctZipperDown terminal node reached")
        uctZipperUp loc (finalResult state) True
    | otherwise =
        uctZipperUp loc' (randomEvalOnce state rGen) False

    where
      state = nodeState $ rootLabel node
      node = tree loc

      loc' = setTree node' loc
      node' = node { subForest = makeSubForest state }

      uctMax = chooseUctMax loc

uctZipperUp  :: (UctNode a) => TreeLoc (UctLabel a) -> Float -> Bool -> ((TreeLoc (UctLabel a)), Bool)
uctZipperUp loc result done =
    case parent loc' of
      Nothing ->
          (loc', done)
      Just parentLoc ->
          -- if evaluating this subtree is finished
          if done
          then
              -- one perfect move is enough to make parent
              -- a losing move
              if result == 1.0
              then
                  -- trace ("uctZipperUp: result 1, done "
                  --            ++ show ("parent", rootLabel parentNode)
                  --            ++ show ("label'", label')
                  --           )
                  uctZipperUp parentLoc 0 True
              else
                  -- if all siblings are also done, then parent
                  -- is also done with 1 - (max of sibling scores)
                  if all (isDone . rootLabel) $ subForest parentNode
                  then
                      -- trace ("uctZipperUp: all done "
                      --        ++ show ("parent", rootLabel parentNode)
                      --        ++ show ("label'", label')
                      --        ++ show ("result''", result'')
                      --        ++ "\n\nsubforest\n"
                      --        ++ (show $ map rootLabel $ subForest parentNode)
                      --       )
                      uctZipperUp parentLoc result'' True
                  else
                      -- trace ("uctZipperUp: done, but active siblings left " ++ show label' ++ (show $ map rootLabel $ filter (not . isDone . rootLabel) $ subForest parentNode))
                      uctZipperUp parentLoc result' False
          else
              uctZipperUp parentLoc result' False
          where
            parentNode = tree parentLoc
            maxResult = winningProb $ rootLabel $ maximum $ subForest parentNode
            result'' = 1 - maxResult
    where
      loc' = setTree node' loc
      node' = node { rootLabel = label' }
      label' = label { winningProb = newProb, visits = (oldCount + 1), isDone = done }

      newProb =
          if done
          then result
          else updateProb oldProb oldCount result
      oldProb = winningProb label
      oldCount = visits label
      label = rootLabel node
      node = tree loc

      result' = 1 - result



makeNodeWithChildren :: (UctNode a) => a -> Tree (UctLabel a)
makeNodeWithChildren state =
    Node { rootLabel = defaultUctLabel { nodeState = state }
         , subForest = makeSubForest state
         }

makeSubForest :: (UctNode a) => a -> [Tree (UctLabel a)]
makeSubForest =
    map makeLeafNode . children

makeLeafNode :: (UctNode a) => a -> Tree (UctLabel a)
makeLeafNode state =
    Node { rootLabel = defaultUctLabel { nodeState = state },
           subForest = [] }



chooseUctMax :: (UctNode a) => TreeLoc (UctLabel a) -> TreeLoc (UctLabel a)
chooseUctMax loc =
  -- trace ("chooseUctMax: "
  --        ++ show ((rootLabel $ tree loc),
  --                 fst uctMaxChildPair,
  --                 (uctValue parentVisits $ fst uctMaxChildPair)))
  uctMaxChild
  where
    uctMaxChild =
        fromJust $ getChild (snd uctMaxChildPair) loc

    uctMaxChildPair =
        maximumBy
        (comparing (uctValue parentVisits . fst))
        activeSubtrees

    parentVisits = visits $ rootLabel $ tree loc

    activeSubtrees =
        filter (not . isDone . fst) numberedForest
    numberedForest = zip (map rootLabel $ subForest (tree loc)) [1..]

uctValue :: (UctNode a) => Int -> (UctLabel a) -> Float
uctValue parentVisits node =
    -- trace ("uctValue: "
    --        ++ show node
    --        ++ show (parentVisits, winningProb node, value))
    value
    where
      value =
          winningProb node
          + (exploratoryC
             * sqrt
             (log (fromIntegral parentVisits)
              / fromIntegral (visits node)))




updateProb :: Float -> Int -> Float -> Float
updateProb oldProb oldCount result =
    ((oldProb * fromIntegral oldCount) + result) / fromIntegral (oldCount + 1)



principalVariation :: (UctNode a) => Tree (UctLabel a) -> [(UctLabel a)]
principalVariation =
    unfoldr maxChild

maxChild :: Tree (UctLabel a) -> Maybe ((UctLabel a), Tree (UctLabel a))
maxChild t =
    case subForest t of
      [] -> Nothing
      forest ->
          Just (rootLabel mNode, mNode)
              where
                mNode = maximum forest






-- inlined Data.Tree.Zipper follows


-- | A position within a 'Tree'.
data TreeLoc a  = Loc
  { tree    :: Tree a       -- ^ The currently selected tree.
  , lefts   :: Forest a     -- ^ Siblings on the left, closest first.
  , rights  :: Forest a     -- ^ Siblings on the right, closest first.
  , parents :: [(Forest a, a, Forest a)]
      -- ^ The contexts of the parents for this location.
  } deriving (Read,Show,Eq)


-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: TreeLoc a -> Maybe (TreeLoc a)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Loc { tree = Node v (combChildren (lefts loc) (tree loc) (rights loc))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing

-- | The top-most parent of the given location.
root :: TreeLoc a -> TreeLoc a
root loc = maybe loc root (parent loc)


-- private: computes the parent for "down" operations.
downParents :: TreeLoc a -> [(Forest a, a, Forest a)]
downParents loc = (lefts loc, rootLabel (tree loc), rights loc) : parents loc



-- | The child with the given index (starting from 0).
getChild :: Int -> TreeLoc a -> Maybe (TreeLoc a)
getChild n loc =
  do (t:ls,rs) <- splitChildren [] (subForest (tree loc)) n
     return Loc { tree = t, lefts = ls, rights = rs, parents = downParents loc }

-- | The first child that satisfies a predicate.
findChild :: (Tree a -> Bool) -> TreeLoc a -> Maybe (TreeLoc a)
findChild p loc =
  do (ls,t,rs) <- split [] (subForest (tree loc))
     return Loc { tree = t, lefts = ls, rights = rs, parents = downParents loc }

  where split acc (x:xs) | p x  = Just (acc,x,xs)
        split acc (x:xs)        = split (x:acc) xs
        split _ []              = Nothing



-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: Tree a -> TreeLoc a
fromTree t = Loc { tree = t, lefts = [], rights = [], parents = [] }

-- | The location of the first tree in a forest.
fromForest :: Forest a -> Maybe (TreeLoc a)
fromForest (t:ts) = Just Loc { tree = t, lefts = [], rights = ts, parents = [] }
fromForest []     = Nothing



-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: TreeLoc a -> Bool
isRoot loc = null (parents loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreeLoc a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isChild :: TreeLoc a -> Bool
isChild loc = not (isRoot loc)

-- | Do we have children?
hasChildren :: TreeLoc a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | Change the current tree.
setTree :: Tree a -> TreeLoc a -> TreeLoc a
setTree t loc = loc { tree = t }

-- | Modify the current tree.
modifyTree :: (Tree a -> Tree a) -> TreeLoc a -> TreeLoc a
modifyTree f loc = setTree (f (tree loc)) loc


-- | Change the label at the current node.
setLabel :: a -> TreeLoc a -> TreeLoc a
setLabel v loc = modifyTree (\t -> t { rootLabel = v }) loc

-- -- Get the current label.
-- getLabel :: TreeLoc a -> a
-- getLabel loc = rootLabel (tree loc)




splitChildren :: [a] -> [a] -> Int -> Maybe ([a],[a])
splitChildren acc xs 0      = Just (acc,xs)
splitChildren acc (x:xs) n  = splitChildren (x:acc) xs $! n-1
splitChildren _ _ _         = Nothing


combChildren :: [b] -> b -> [b] -> [b]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls
