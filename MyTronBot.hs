{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import System.Random (StdGen, newStdGen, randomR)
import System.Time (ClockTime, TimeDiff(..), getClockTime, diffClockTimes)

import Control.Monad (replicateM, liftM)
import Control.Monad.ST (ST, runST)

import Data.List (foldl', unfoldr, maximumBy, transpose, intersect)

import Data.Maybe (fromJust)
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
exploratoryConstant = 0.2

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
  tronMap <- return $ makeTronMap w h str
  -- gameState <- return $ newGameState w h str rGen
  -- move <- bot gameState
  move <- runBot tronMap w h rGen
  putStrLn $ makeMove move
  playTurns bot


makeTronMap :: Int -> Int -> String -> TronMap
makeTronMap w h str =
    array
    ((1, 1), (w, h))
    $ zip [(x,y) | y <- [1 .. h], x <- [1 .. w]] str


runBot :: TronMap -> Int -> Int -> StdGen -> IO Move
runBot tronMap w h rGen =
    case floodFill tronMap pPos of
      Left MetOther ->
          uctBot
          GameState { getTronMap = tronMap
                    , ourRandomGen = rGen
                    , moveHistory = []
                    , playerCrashed = False
                    , enemyCrashed = False
                    , playerPos = pPos
                    , enemyPos = initPos tronMap Enemy
                    , maxX = w
                    , maxY = h
                    , toMove = Player
                    , divided = False
                    }
      Right pA ->
          uctBotEnd
          EndGameState { getTronMapEnd = tronMap
                       , moveHistoryEnd = []
                       , ourRandomGenEnd = rGen
                       , playerCrashedEnd = False
                       , playerPosEnd = pPos
                       , maxXEnd = w
                       , maxYEnd = h
                       , maxArea = pA
    }
    where
      pPos = initPos tronMap Player


uctBot :: GameState -> IO Move
uctBot state =
    case possibleMoves tronMap pPos of
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
      where
        tronMap = getTronMap state
        pPos = playerPos state

uctBotEnd :: EndGameState -> IO Move
uctBotEnd state =
    case possibleMoves tronMap pPos of
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
                             -- ++ (showTronMap (debugUct t (getTronMapEnd state)))
                             -- ++ "\n"
                             ++ (alternateFirstMoves t)
                            )
                      moveFromPvEnd pv))
      where
        tronMap = getTronMapEnd state
        pPos = playerPosEnd state


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
      h = snd $ last $ indices t
      maybeSplit "" = Nothing
      maybeSplit str = Just $ splitAt h str

moveFromPv :: [UctLabel GameState] -> Move
moveFromPv pv =
    snd $ last $ moveHistory $ nodeState $ head pv

moveFromPvEnd :: [UctLabel EndGameState] -> Move
moveFromPvEnd pv =
    last $ moveHistoryEnd $ nodeState $ head pv


-- uct :: (UctNode a) => a -> IO (Tree (UctLabel a))
uct :: (UctNode a) => a -> IO (Tree (UctLabel a))
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
     ,divided       :: Bool
    }

data EndGameState = EndGameState {
      getTronMapEnd      :: TronMap
    , moveHistoryEnd     :: [Move]
    , ourRandomGenEnd    :: StdGen
    , playerCrashedEnd   :: Bool
    , playerPosEnd       :: (Int,Int)
    , maxXEnd            :: Int
    , maxYEnd            :: Int
    , maxArea            :: Int
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

    randomEvalOnce state rGen =
        runOneRandom state rGen
        -- if divided state
        -- then runOneRandom state rGen
        -- else
        --     case floodFill tronMap pPos of
        --       Left MetOther ->
        --           runOneRandom state rGen
        --       Right _n ->
        --           areaHeuristic state
        -- where
        --   tronMap = getTronMap state
        --   pPos = playerPos state

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
        (distanceHeuristic state, 1000)
        -- if divided state
        -- then (0.5, 1)
        --     -- (areaHeuristic state, 10000)
        --     -- (fillerHeuristic state, 100)
        -- else

        -- -- (0.5, 1)
        -- case floodFill tronMap pPos of
        --   Left MetOther ->
        --       (distToHeuristic dist size, 1000)
        --   Right pArea ->
        --       case floodFill tronMap ePos of
        --         Left MetOther ->
        --             error "floodFill from enemy finds player when floodFill from player does not find enemy"
        --         Right eArea ->
        --             (areasToHeuristic
        --              (fromIntegral pArea)
        --              (fromIntegral eArea),
        --              1000)


instance Show EndGameState where
    show state =
        case moveHistoryEnd state of
          [] -> ""
          moves ->
              show $ reverse moves

instance UctNode EndGameState where
    isTerminalNode state =
        playerCrashedEnd state

    finalResult state =
        finalResultEnd' state

    randomEvalOnce state rGen =
        runOneRandomEnd state rGen
        -- if divided state
        -- then runOneRandom state rGen
        -- else
        --     case floodFill tronMap pPos of
        --       Left MetOther ->
        --           runOneRandom state rGen
        --       Right _n ->
        --           areaHeuristic state
        -- where
        --   tronMap = getTronMap state
        --   pPos = playerPos state

    children state =
        -- trace ("children " ++
        --        (show (state, moves)))
        map (updateEndGameState state) moves
        where
          moves =
              case possibleMoves
                   tronMap
                   (playerPosEnd state) of
                [] -> [North]
                ls -> ls
          tronMap = getTronMapEnd state

    heuristic state =
        case floodFill
                 (getTronMapEnd state)
                 (playerPosEnd state) of
          Left MetOther ->
              error "randomEvalOnce player MetOther in endgame"
          Right pA ->
              ((runOneRandomEnd state rGen / fromIntegral pA), 1)
        where
          rGen = ourRandomGenEnd state


-- areaHeuristic :: GameState -> Float
-- areaHeuristic state =
--     case floodFill tronMap (playerPos state) of
--       Left MetOther ->
--           error "randomEvalOnce player MetOther when divided"
--       Right pA ->
--           case floodFill tronMap (enemyPos state) of
--             Left MetOther ->
--                 error "randomEvalOnce enemy MetOther when divided"
--             Right eA ->
--                 if diff > 5
--                 then 1.0
--                 else if diff < -5
--                      then 0.0
--                      else 0.5 + (diff / 10)
--                 where
--                   diff = fromIntegral $ pA - eA
--      where
--       tronMap = getTronMap state

-- fillerHeuristic :: GameState -> Float
-- fillerHeuristic state =
--     if 2 == (length $ possibleMoves (getTronMap state) (playerPos state))
--     then 0.75
--     else 0.25

distanceHeuristic :: GameState -> Float
distanceHeuristic state =
    distToHeuristic dist size
    where
      size = fromIntegral $
             -- maxX state + maxY state
             ((maxX state) - 2) ^ (2 :: Int)
             + ((maxY state) - 2) ^ (2 :: Int)
      dist = fromIntegral $
             euclidianDistance pPos ePos
             -- (manhattanDistance pPos ePos) ^ (2 :: Int)
      pPos = playerPos state
      ePos = enemyPos state

distToHeuristic :: Float -> Float -> Float
distToHeuristic d s =
    -- trace ("distToHeuristic " ++ show (d, s, h))
    h
    where
      h =
          if d < 3
          then 0.5
          else 0.8 - (d / s) ^ (3 :: Int) / 3
          -- 1.0 - d ^ (2 :: Int) / s ^ (2 :: Int) / 2

-- manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
-- manhattanDistance (x1, y1) (x2, y2) =
--     max (abs (x1 - x2)) (abs (y1 - y2))
--     -- abs (x1 - x2) + abs (y1 - y2)

euclidianDistance :: (Int, Int) -> (Int, Int) -> Int
euclidianDistance (x1, y1) (x2, y2) =
    (abs (x1 - x2))^(2 :: Int) + (abs (y1 - y2))^(2 :: Int)



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


runOneRandomEnd :: EndGameState -> StdGen -> Float
runOneRandomEnd initState initGen =
    -- trace ("runOneRandom " ++ showMap (getTronMap initState))
    run initState initGen 0
    where
      run :: EndGameState -> StdGen -> Int -> Float
      run _ _ 10000 = 1.0
      run state rGen runCount =
          if isTerminalNode state'
          then
              -- trace ("runOneRandomBiased " ++ show runCount ++ "\n"
              --         ++ (showTronMap (getTronMapEnd state')) ++ "\n"
              --         ++ (show (moveHistoryEnd state')))
              finalResultEnd' state'
          else
              run state' rGen' (runCount + 1)
          where
            state' = updateEndGameState state move
            (move, rGen') =
                genMoveRandEnd state rGen allMovesOrdered
      (allMovesOrdered, _) = 
          pick [ [North, South, East, West]
               , [South, North, East, West]
               , [North, South, West, East]
               , [South, North, West, East]
               , [East, West, North, South]
               , [West, East, North, South]
               , [East, West, South, North]
               , [West, East, South, North]
               ] initGen

genMoveRandEnd :: EndGameState -> StdGen -> [Move]
                  -> (Move, StdGen)
genMoveRandEnd state rGen allMovesOrdered =
    -- trace ("\n\ngenMoveRand" ++
    --        showMap tronMap ++
    --        show (who, move, moveList))
    (move, rGen')
    where
      (move, rGen') =
          case moveList of
            [] -> (North, rGen)
            [singleMove] -> (singleMove, rGen)
            moves ->
                -- (head moves, rGen)
                case filter (not . (oneWayMove tronMap pPos)) moves of
                  [] ->
                      pick moves rGen
                  [singleMove] ->
                      (singleMove, rGen)
                  moves' ->
                      (head moves', rGen)
                -- pick moves' rGen
                -- where
                --   moves' =
                --       concatMap rIfTwo moves
                --   rIfTwo m =
                --       if 2 == (length $ possibleMoves tronMap (updatePos mPos m))
                --       then replicate 3 m
                --       else [m]
      moveList = allMovesOrdered `intersect` possibleMoves tronMap pPos
      pPos = playerPosEnd state
      tronMap = getTronMapEnd state


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


updateEndGameState :: EndGameState -> Move -> EndGameState
updateEndGameState state move =
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
          state { getTronMapEnd = tronMap'
                , moveHistoryEnd = moveHistory'
                , playerCrashedEnd = playerCrashed'
                , playerPosEnd = playerPos'
                }

      playerCrashed' = crashed tronMap playerPos'

      playerPos' = updatePos (playerPosEnd state) move

      tronMap' = tronMap // updates
      updates = [(playerPosEnd state, (showSpot Wall)),
                 (playerPos', (showSpot Player))]

      moveHistory' = moveHistoryEnd state ++ [move]
      tronMap = getTronMapEnd state




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

finalResultEnd' :: EndGameState -> Float
finalResultEnd' state =
    (fromIntegral $ length $ moveHistoryEnd state)
    / (fromIntegral $ maxArea state)



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
           (if null $ eitherLefts [n,s,e,w]
            then return $ Right (1 + (sum $ eitherRights [n,s,e,w]))
            else return $ Left MetOther))
     other -> error ("floodFill' encountered " ++ show other))

eitherLefts   :: [Either a b] -> [a]
eitherLefts x = [a | Left a <- x]

eitherRights   :: [Either a b] -> [b]
eitherRights x = [a | Right a <- x]


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

oneWayMove :: TronMap -> (Int, Int) -> Move -> Bool
oneWayMove tronMap p m =
    case possibleMoves tronMap p' of
      [] -> True
      [m'] ->
          case possibleMoves tronMap (updatePos p' m') of
            [] -> True
            [m''] -> trace ("oneWayMove " ++ show m'') True
            _otherwise -> False
      _otherwise -> False
    where
      p' = updatePos p m

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
    randomEvalOnce :: a -> StdGen -> Float
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





uctZipperDown  :: (UctNode a) => TreeLoc (UctLabel a) -> StdGen -> ((TreeLoc (UctLabel a)), Bool)
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
