{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import qualified System.Random as R (split)
import System.Random (StdGen, RandomGen, newStdGen, randomR)
import System.Time (ClockTime, TimeDiff(..), getClockTime, diffClockTimes)

import Control.Monad (replicateM, liftM)
import Control.Monad.ST (ST, runST)

import Data.List (foldl', unfoldr, maximumBy, transpose)

import Data.Maybe (fromJust)
import qualified Data.Either as E (lefts, rights)
import Data.Tree (Tree(..), Forest, flatten)
import Data.Ord (comparing)
import Text.Printf (printf)

-- import Data.Array.Diff
import Data.Array.Unboxed (UArray, array, (//), (!), assocs, indices, elems)
import Data.Array.ST (STUArray, thaw, readArray, writeArray)

import Debug.Trace (trace)
import Data.List (sort)

type TronMap = UArray (Int, Int) Char
-- type TronMap = DiffUArray (Int, Int) Char



-- constants
exploratoryConstant :: Float
exploratoryConstant = 0.5

uctTimePerMove :: TimeDiff
uctTimePerMove = TimeDiff {
                   -- tdPicosec = 900000000000
                   tdPicosec = 900000000000
                 , tdYear = 0
                 , tdMonth = 0
                 , tdDay = 0
                 , tdHour = 0
                 , tdMin = 0
                 , tdSec = 0
                 }



setBuffers :: IO ()
setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

main :: IO ()
main = playBot uctBot
                                     
playBot :: (GameState -> IO Move) -> IO ()
playBot bot = do
    setBuffers
    playTurns bot

playTurns :: (GameState -> IO Move) -> IO ()
playTurns bot = do
  sizeLine <- getLine
  [w,h] <- return $ map readInt (words sizeLine)
  -- str <- getContents >>= (return . concat . take h . lines)
  str <- liftM concat $ replicateM h getLine
  rGen <- newStdGen
  gameState <- return $ newGameState w h str rGen
  move <- bot gameState
  putStrLn $ makeMove move
  playTurns bot
    where 
      newGameState w h str rGen =
          GameState { getTronMap = tronMap
                    , ourRandomGen = rGen
                    , moveHistory = []
                    , playerCrashed = False
                    , enemyCrashed = False
                    , playerPos = initPos tronMap Player
                    , enemyPos = initPos tronMap Enemy
                    , maxX = w
                    , maxY = h
                    , toMove = Player
                    }
          where
            tronMap =
                array
                ((1, 1), (w, h))
                $ zip [(x,y) | y <- [1 .. h], x <- [1 .. w]] str


uctBot :: GameState -> IO Move
uctBot state =
    case possibleMoves
             (getTronMap state)
             (playerPos state) of
      [] ->
          -- trace "uctBot no possible moves, go North"
                return North
      [onlyMove] ->
          -- trace ("uctBot only one move: " ++ show onlyMove)
                return onlyMove
      _otherwise ->
          (do
            t <- uct state
            pv <- return $ principalVariation t
            -- return $ moveFromPv pv)
            return $ (
                      trace ("uctBot\n"
                             ++ (showTronMap (debugUct t (getTronMap state)))
                             ++ "\n"
                             ++ (alternateFirstMoves t)
                            )
                      moveFromPv pv))

alternateFirstMoves :: (UctNode a) => Tree (UctLabel a) -> String
alternateFirstMoves t =
    show $ map rootLabel $ reverse $ sort $ subForest t

debugUct :: Tree (UctLabel GameState) -> TronMap -> TronMap
debugUct initTree initTronMap =
    foldl' updateMap' t' $
           map nodeState $ principalVariation initTree
    where
      t' =
          foldl' updateMap initTronMap $
                 map nodeState $ flatten initTree

      updateMap :: TronMap -> GameState -> TronMap
      updateMap t state =
          t // [(pPos, pV), (ePos, eV)]
          where
            pPos = playerPos state
            ePos = enemyPos state
            pV = case t ! pPos of
                   ' ' -> 'p'
                   'p' -> 'P'
                   'P' -> 'P'
                   'e' -> 'W'
                   'E' -> 'W'
                   c -> c
            eV = case t ! ePos of
                   ' ' -> 'e'
                   'e' -> 'E'
                   'E' -> 'E'
                   'p' -> 'W'
                   'P' -> 'W'
                   c -> c

      updateMap' :: TronMap -> GameState -> TronMap
      updateMap' t state =
          t // if pPos == ePos
               then [(pPos, 'M')]
               else [(pPos, '1'), (ePos, '2')]
          where
            pPos = playerPos state
            ePos = enemyPos state


showTronMap :: TronMap -> String
showTronMap t =
    unlines $ transpose $ unfoldr maybeSplit es
    where
      es = elems t
      h = fst $ last $ indices t
      maybeSplit "" = Nothing
      maybeSplit str = Just $ splitAt h str

moveFromPv :: [UctLabel GameState] -> Move
moveFromPv pv =
    snd $ last $ moveHistory $ nodeState $ head pv


-- uct :: (UctNode a) => a -> IO (Tree (UctLabel a))
uct :: GameState -> IO (Tree (UctLabel GameState))
uct initState =
    -- trace ("uctBot\n"
    --        -- ++ show ("heuristic",(heuristic initState)) ++ "\n"
    --        -- ++ show ("floo",
    --        --          (floodFill
    --        --           (getTronMap initState)
    --        --           (playerPos initState))) ++ "\n"
    --       )
    getClockTime >>= uctZipper (fromTree $ makeNodeWithChildren initState)

uctZipper :: (UctNode a) =>
             TreeLoc (UctLabel a) ->
             ClockTime ->
             IO (Tree (UctLabel a))
uctZipper loc startTime = do
  rGen <- newStdGen
  (loc', done) <- return $ uctZipperDown loc rGen
  (if done
   then return $ tree loc'
   else (do
          now <- getClockTime
          (if (diffClockTimes now startTime) > uctTimePerMove
           then return $ tree loc'
           else uctZipper loc' startTime)))





--------------------------------------------
-- GameState / tron / game specific code
--------------------------------------------

data Spot = Wall | Blank | Player | Enemy deriving Eq
data Move = North | East | South | West deriving (Eq, Show)

allMoves :: [Move]
allMoves = [North, East, South, West]

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
      return $ runOneRandom (state {ourRandomGen = rGen'}) rGen
      where
        (rGen, rGen') = R.split $ ourRandomGen state

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

    heuristic state =
        -- (0.5, 1)
        case floodFill tronMap pPos of
          Left MetOther ->
              (distToHeuristic dist size, 1000)
          Right pArea ->
              case floodFill tronMap ePos of
                Left MetOther ->
                    error "floodFill from enemy finds player when floodFill from player does not find enemy"
                Right eArea ->
                    (areasToHeuristic
                     (fromIntegral pArea)
                     (fromIntegral eArea),
                     1000)
        where
          size = fromIntegral $
                 -- maxX state + maxY state
                 ((maxX state) - 2) ^ (2 :: Int)
                 + ((maxY state) - 2) ^ (2 :: Int)
          dist = fromIntegral $
                 euclidianDistance pPos ePos
                 -- (manhattanDistance pPos ePos) ^ (2 :: Int)
          tronMap = getTronMap state
          pPos = playerPos state
          ePos = enemyPos state

distToHeuristic :: Float -> Float -> Float
distToHeuristic d s =
    -- trace ("distToHeuristic " ++ show (d, s, h))
    h
    where
      h =
          0.5 - (d / s) ^ (2 :: Int) / 3
          -- 1.0 - d ^ (2 :: Int) / s ^ (2 :: Int) / 2

areasToHeuristic :: Float -> Float -> Float
areasToHeuristic pA eA =
    pA / (pA + eA)

runOneRandom :: GameState -> StdGen -> Float
runOneRandom initState initGen =
    -- trace ("runOneRandom " ++ showMap (getTronMap initState))
    run initState initGen 0
    where
      run :: GameState -> StdGen -> Int -> Float
      run _ _ 1000 =
          -- trace "run returning after 1000 iterations"
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


manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) =
    max (abs (x1 - x2)) (abs (y1 - y2))
    -- abs (x1 - x2) + abs (y1 - y2)

euclidianDistance :: (Int, Int) -> (Int, Int) -> Int
euclidianDistance (x1, y1) (x2, y2) =
    (abs (x1 - x2))^(2 :: Int) + (abs (y1 - y2))^(2 :: Int)


data MetOther = MetOther deriving (Eq, Show)

floodFill :: TronMap -> (Int, Int) -> Either MetOther Int
floodFill initTronMap initP =
    runST $ (do a <- thaw initTronMap
                writeArray a initP ' '
                floodFill' a initP)

floodFill' :: (STUArray s (Int, Int) Char) -> (Int, Int) -> ST s (Either MetOther Int)
floodFill' a p@(x, y) = do
  v <- readArray a p
  (case v of
     'X' -> return $ Right 0
     '#' -> return $ Right 0
     '2' -> return $ Left MetOther
     '1' -> return $ Left MetOther
     ' ' ->
         (do
           writeArray a p 'X'
           n <- floodFill' a (x, y-1)
           s <- floodFill' a (x, y+1)
           e <- floodFill' a (x+1, y)
           w <- floodFill' a (x-1, y)
           (if null $ E.lefts [n,s,e,w]
            then return $ Right (1 + (sum $ E.rights [n,s,e,w]))
            else return $ Left MetOther))
     other -> error ("floodFill' encountered " ++ show other))

--     floodFill' initTronMap initP
--     where
--       floodFill' :: TronMap -> (Int, Int) -> Maybe Int
--       floodFill' tronMap p
--           | tronMap ! p == '2' = Nothing
--           | tronMap ! p == ' ' =
--               foldl' f (tronMap', Just 0) allMoves
--               where
--                 f :: (Maybe Int) -> Move -> (Maybe Int)
--                 f n move =
--                     (floodFill' tronMap' (updatePos p move))
--                 tronMap' = tronMap // (p, 'X')
--           | otherwise = Just 0

 --    Flood-fill (node, target-color, replacement-color):
 -- 1. If the color of node is not equal to target-color, return.
 -- 2. Set the color of node to replacement-color.
 -- 3. Perform Flood-fill (one step to the west of node, target-color, replacement-color).
 --    Perform Flood-fill (one step to the east of node, target-color, replacement-color).
 --    Perform Flood-fill (one step to the north of node, target-color, replacement-color).
 --    Perform Flood-fill (one step to the south of node, target-color, replacement-color).
 -- 4. Return




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
          filter possibleMove allMoves
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



readInt :: String -> Int
readInt = read

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





-----------------------------------------
-- Data.Tree.UCT uct code
-----------------------------------------



class (Show a) => UctNode a where
    isTerminalNode :: a -> Bool
    finalResult :: a -> Float
    heuristic :: a -> (Float, Int)
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

newUctLabel :: (UctNode a) => a -> UctLabel a
newUctLabel state = UctLabel {
                      nodeState = state
                    , winningProb = initProb
                    , visits = heuristicWeight
                    , isDone = False
                    }
    where
      (initProb, heuristicWeight) = heuristic state

-- defaultUctLabel :: UctLabel a
-- defaultUctLabel = UctLabel {
--                     nodeState = undefined
--                   , winningProb = 0.5
--                   -- , uctValue = 0.5
--                   , visits = 1
--                   , isDone = False
--                   }





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
    Node { rootLabel = newUctLabel state
         , subForest = makeSubForest state
         }

makeSubForest :: (UctNode a) => a -> [Tree (UctLabel a)]
makeSubForest =
    map makeLeafNode . children

makeLeafNode :: (UctNode a) => a -> Tree (UctLabel a)
makeLeafNode state =
    Node { rootLabel = newUctLabel state
         , subForest = [] }



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
    --        ++ show (
    --                 ("winningProb node",(winningProb node)),
    --                 ("b", b),
    --                 ("sqrt", sqrt (log (fromIntegral parentVisits) / fromIntegral (visits node))),
    --                 ("log", (log (fromIntegral parentVisits)),
    --                 ("parentVisits",parentVisits),
    --                 ("parentVisits",parentVisits),
    --                 ("visits node",(visits node)),

    --                 value)))
    value
    where
      value =
          winningProb node
          + b
      b =
          (exploratoryConstant
           * sqrt
           ((log (fromIntegral parentVisits))
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







-----------------------------------------------
-- inlined Data.Tree.Zipper follows
-----------------------------------------------

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


-- private: computes the parent for "down" operations.
downParents :: TreeLoc a -> [(Forest a, a, Forest a)]
downParents loc = (lefts loc, rootLabel (tree loc), rights loc) : parents loc


-- | The child with the given index (starting from 0).
getChild :: Int -> TreeLoc a -> Maybe (TreeLoc a)
getChild n loc =
  do (t:ls,rs) <- splitChildren [] (subForest (tree loc)) n
     return Loc { tree = t, lefts = ls, rights = rs, parents = downParents loc }


-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: Tree a -> TreeLoc a
fromTree t = Loc { tree = t, lefts = [], rights = [], parents = [] }


-- Queries ---------------------------------------------------------------------

-- | Are we at the bottom of the tree?
isLeaf :: TreeLoc a -> Bool
isLeaf loc = null (subForest (tree loc))


-- | Do we have children?
hasChildren :: TreeLoc a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------

-- | Change the current tree.
setTree :: Tree a -> TreeLoc a -> TreeLoc a
setTree t loc = loc { tree = t }


splitChildren :: [a] -> [a] -> Int -> Maybe ([a],[a])
splitChildren acc xs 0      = Just (acc,xs)
splitChildren acc (x:xs) n  = splitChildren (x:acc) xs $! n-1
splitChildren _ _ _         = Nothing


combChildren :: [b] -> b -> [b] -> [b]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls
