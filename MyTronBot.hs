
{-# LANGUAGE FlexibleInstances #-}

import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import System.Random (StdGen, newStdGen, randomR)
import System.Time (ClockTime, TimeDiff(..), getClockTime, diffClockTimes)

import Control.Monad (replicateM, liftM, filterM)
import Control.Monad.ST (ST, runST)

import Data.List (foldl', unfoldr, maximumBy, transpose, intersect)
import Data.List (sort)

import Data.Maybe (fromJust)
import Data.Tree (Tree(..), Forest, flatten)
import Data.Ord (comparing)
import Text.Printf (printf)

-- import Data.Array.Diff
import qualified Data.IntMap as M ()
import Data.Array.Unboxed (UArray, array, (//), (!), assocs, indices, elems)
import Data.Array.ST (STUArray, thaw, readArray, writeArray)

-- import Debug.Trace (trace)

maybeTrace :: String -> b -> b
-- maybeTrace = trace
maybeTrace _ = id


type TronMap = UArray (Int, Int) Char
-- type TronMap = DiffUArray (Int, Int) Char



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
    if metOther
    then
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
                  , maxArea = pA
                  , toMove = Player
                  }
    else
        uctBotEnd
        EndGameState { getTronMapEnd = tronMap
                     , moveHistoryEnd = []
                     , ourRandomGenEnd = rGen
                     , playerCrashedEnd = False
                     , playerPosEnd = pPos
                     , maxXEnd = w
                     , maxYEnd = h
                     , maxAreaEnd = pA
    }
    where
      (metOther, pA) = floodFill tronMap pPos
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
                      maybeTrace ("uctBot\n"
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
                      maybeTrace ("uctBotEnd\n"
                             ++ (showTronMap (debugUctEnd t (getTronMapEnd state)))
                             ++ "\n"
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

debugUctEnd :: Tree (UctLabel EndGameState) -> TronMap -> TronMap
debugUctEnd initTree initTronMap =
    foldl' updateMap' t' $
           map nodeState $ principalVariation initTree
    where
      t' =
          foldl' updateMap initTronMap $
                 map nodeState $ flatten initTree

      updateMap :: TronMap -> EndGameState -> TronMap
      updateMap t state =
          t // [(pPos, pV)]
          where
            pPos = playerPosEnd state
            pV = case t ! pPos of
                   ' ' -> 'p'
                   'p' -> 'P'
                   'P' -> 'P'
                   'e' -> 'W'
                   'E' -> 'W'
                   c -> c

      updateMap' :: TronMap -> EndGameState -> TronMap
      updateMap' t state =
          t // [(pPos, '1')]
          where
            pPos = playerPosEnd state



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
     ,maxArea         :: Int
    }

data EndGameState = EndGameState {
      getTronMapEnd      :: TronMap
    , moveHistoryEnd     :: [Move]
    , ourRandomGenEnd    :: StdGen
    , playerCrashedEnd   :: Bool
    , playerPosEnd       :: (Int,Int)
    , maxXEnd            :: Int
    , maxYEnd            :: Int
    , maxAreaEnd         :: Int
    }


instance Show GameState where
    show state =
        case moveHistory state of
          [] -> ""
          moves ->
              show $ reverse moves

instance UctNode GameState where
    exploratoryConstant _state = 0.5

    isTerminalNode state =
        -- trace ("isTerminalNode " ++
        --        (show (state, ("complete",complete), ("crashes",anyCrashes), result)))
        result
        where
          result = complete && anyCrashes || divided
          complete = completeRound state
          anyCrashes =
              playerCrashed state || enemyCrashed state
          divided = not . fst $ floodFill tronMap pPos
          tronMap = getTronMap state
          pPos = playerPos state
              

    finalResult state =
        if pC || eC
        then
            finalResult' pC eC (lastToMove state)
        else
            if (lastToMove state) == Player
            then res
            else 1 - res

        where
          res = if diff > 5
                then 1.0
                else if diff < -5
                     then 0.0
                     else 0.5 + (diff / 20)
          diff = fromIntegral pA - fromIntegral eA

          (_, pA) = floodFill tronMap pPos
          (_, eA) = floodFill tronMap ePos

          tronMap = getTronMap state
          pPos = playerPos state
          ePos = playerPos state
          pC = playerCrashed state
          eC = enemyCrashed state



    randomEvalOnce state rGen =
        runOneRandomST state rGen


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

    updateResult _state result =
        1 - result


instance Show EndGameState where
    show state =
        case moveHistoryEnd state of
          [] -> ""
          moves ->
              show $ reverse moves

instance UctNode EndGameState where
    exploratoryConstant _state = 1.0

    isTerminalNode state =
        playerCrashedEnd state

    finalResult state =
        finalResultEnd' moveCount maxCount
        where
          moveCount = length $ moveHistoryEnd state
          maxCount = maxAreaEnd state


    randomEvalOnce state rGen =
        runOneRandomSTEnd state rGen
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
        (((runOneRandomSTEnd state rGen) + fromIntegral pA)
         / (fromIntegral (maxAreaEnd state)) / 2, 10)
        where
          (_, pA) =
              floodFill (getTronMapEnd state) (playerPosEnd state)
          rGen = ourRandomGenEnd state

    updateResult _state = id

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



runOneRandomST :: GameState -> StdGen -> Float
runOneRandomST initState initGen =
    -- trace ("runOneRandom " ++ showMap (getTronMap initState))
    runST $ (do initArray <- thaw initTronMap
                run initArray initGen 0 initPPos initEPos)

    where
      run :: (STUArray s (Int, Int) Char) -> StdGen -> Int
          -> (Int, Int) -> (Int, Int)
          -> ST s Float
      run _ _ 1000 _ _ =
          -- trace "run returning after 1000 iterations"
          return 0.5
      run tronArray rGen runCount pPos ePos = do
        moveList <- possibleMovesST tronArray pPos
        (pMove, rGen') <- return $ genMoveRand moveList rGen
        moveList' <- possibleMovesST tronArray ePos
        (eMove, rGen'') <- return $ genMoveRand moveList' rGen'
        pPos' <- return $ updatePos pPos pMove
        ePos' <- return $ updatePos ePos eMove
        pCrashed <- crashedST tronArray pPos'
        eCrashed <- crashedST tronArray ePos'
        bothCrashed <- return $ pPos' == ePos'
        pCrashed' <- return $ pCrashed || bothCrashed
        eCrashed' <- return $ eCrashed || bothCrashed
        writeArray tronArray pPos (showSpot Wall)
        writeArray tronArray pPos (showSpot Player)
        writeArray tronArray ePos (showSpot Wall)
        writeArray tronArray ePos (showSpot Enemy)
        (if pCrashed' || eCrashed'
          then
              return $ finalResult' pCrashed' eCrashed' initWho
          else
              run tronArray rGen'' (runCount + 1) pPos' ePos')

      initWho = lastToMove initState
      initTronMap = getTronMap initState
      initPPos = playerPos initState
      initEPos = enemyPos initState



runOneRandomSTEnd :: EndGameState -> StdGen -> Float
runOneRandomSTEnd initState initGen =
    runST $ (do initArray <- thaw initTronMap
                run initArray initGen 0 initPPos)
    where
      run :: (STUArray s (Int, Int) Char) -> StdGen -> Int
          -> (Int, Int)
          -> ST s Float
      run _ _ 10000 _ =
          return 1.0
      run tronArray rGen runCount pPos = do
        moveList <- possibleMovesST tronArray pPos
        moveList' <- return $ allMovesOrdered `intersect` moveList
        (pMove, rGen') <- (case moveList' of
                            [] ->
                                return (North, rGen)
                            [singleMove] ->
                                return (singleMove, rGen)
                            moves -> (do
                              moves' <- filterM (notOneWayMoveST tronArray pPos) moveList'
                              case moves' of
                                [] -> return $ pick moves rGen
                                [singleMove] ->
                                    return $ (singleMove, rGen)
                                moves'' -> return $
                                           (head moves'', rGen)
                                     ))
        pPos' <- return $ updatePos pPos pMove
        pCrashed <- crashedST tronArray pPos'
        writeArray tronArray pPos (showSpot Wall)
        writeArray tronArray pPos (showSpot Player)
        (if pCrashed
          then
              return $ finalResultEnd' runCount initMaxArea
          else
              run tronArray rGen' (runCount + 1) pPos')

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

      initTronMap = getTronMapEnd initState
      initPPos = playerPosEnd initState
      initMaxArea = maxAreaEnd initState



possibleMovesST :: (STUArray s (Int, Int) Char) -> (Int, Int) -> ST s [Move]
possibleMovesST tronArray position =
    filterM (possibleMoveST tronArray position) allMoves

possibleMoveST :: (STUArray s (Int, Int) Char) -> (Int, Int) -> Move -> ST s Bool
possibleMoveST tronArray position move = do
        v <- readArray tronArray (updatePos position move)
        return $ v /= showSpot Wall

crashedST :: (STUArray s (Int, Int) Char) -> (Int, Int) -> ST s Bool
crashedST tronArray pos = do
  v <- readArray tronArray pos
  return $ v /= showSpot Blank

notOneWayMoveST :: (STUArray s (Int, Int) Char) -> (Int, Int) -> Move -> ST s Bool
notOneWayMoveST tronArray p m = do
  p' <- return $ updatePos p m
  pMoves <- possibleMovesST tronArray p'
  case pMoves of
    [] -> return False
    [m'] ->
        (do
          p'' <- return $ updatePos p' m'
          pMoves' <- possibleMovesST tronArray p''
          case pMoves' of
            [] -> return False
            [_m''] -> return False
            _otherwise -> return True)
    _otherwise -> return True



-- runOneRandom :: GameState -> StdGen -> Float
-- runOneRandom initState initGen =
--     -- trace ("runOneRandom " ++ showMap (getTronMap initState))
--     run initState initGen 0
--     where
--       run :: GameState -> StdGen -> Int -> Float
--       run _ _ 1000 =
--           -- trace "run returning after 1000 iterations"
--           0.5
--       run state rGen runCount =
--           if isTerminalNode state'
--           then
--               finalResult'
--               (playerCrashed state')
--               (enemyCrashed state')
--               who
--           else
--               run state' rGen' (runCount + 1)
--           where
--             state' = updateGameState state move
--             (move, rGen') = genMoveRand moveList rGen
--             moveList = possibleMoves tronMap (moverPos state)
--             tronMap = getTronMap state

--       who = lastToMove initState


genMoveRand :: [Move] -> StdGen -> (Move, StdGen)
genMoveRand moveList rGen =
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


-- runOneRandomEnd :: EndGameState -> StdGen -> Float
-- runOneRandomEnd initState initGen =
--     -- trace ("runOneRandom " ++ showMap (getTronMap initState))
--     run initState initGen 0
--     where
--       run :: EndGameState -> StdGen -> Int -> Float
--       run _ _ 10000 = 1.0
--       run state rGen runCount =
--           if isTerminalNode state'
--           then
--               -- trace ("runOneRandomBiased " ++ show runCount ++ "\n"
--               --         ++ (showTronMap (getTronMapEnd state')) ++ "\n"
--               --         ++ (show (moveHistoryEnd state')))
--               finalResultEnd' runCount initMaxArea
--           else
--               run state' rGen' (runCount + 1)
--           where
--             state' = updateEndGameState state move
--             (move, rGen') =
--                 genMoveRandEnd moveList rGen pPos tronMap
--             moveList =
--                 allMovesOrdered
--                 `intersect`
--                 possibleMoves tronMap pPos
--             tronMap = getTronMapEnd state
--             pPos = playerPosEnd state

--       (allMovesOrdered, _) = 
--           pick [ [North, South, East, West]
--                , [South, North, East, West]
--                , [North, South, West, East]
--                , [South, North, West, East]
--                , [East, West, North, South]
--                , [West, East, North, South]
--                , [East, West, South, North]
--                , [West, East, South, North]
--                ] initGen

--       initMaxArea = maxAreaEnd initState


-- genMoveRandEnd :: [Move] -> StdGen -> (Int,Int) -> TronMap
--                   -> (Move, StdGen)
-- genMoveRandEnd moveList rGen pPos tronMap =
--     (move, rGen')
--     where
--       (move, rGen') =
--           case moveList of
--             [] -> (North, rGen)
--             [singleMove] -> (singleMove, rGen)
--             moves ->
--                 -- (head moves, rGen)
--                 case filter (not . (oneWayMove tronMap pPos)) moves of
--                   [] ->
--                       pick moves rGen
--                   [singleMove] ->
--                       (singleMove, rGen)
--                   moves' ->
--                       (head moves', rGen)


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




finalResult' :: Bool -> Bool -> Spot -> Float
finalResult' pCrashed eCrashed who =
        -- trace ("finalResult2 "
        --        ++ show (((playerCrashed state), (enemyCrashed state)),who,result)
        --        ++ showMap (getTronMap state)
        --        -- show moveHistory state) ++
        --       )
        result
        where
          result =
              case (pCrashed, eCrashed) of
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


finalResultEnd' :: Int -> Int -> Float
finalResultEnd' moveCount maxCount =
    fromIntegral moveCount / fromIntegral maxCount




data MetOther = MetOther deriving (Eq, Show)

floodFill :: TronMap -> (Int, Int) -> (Bool, Int)
floodFill initTronMap initP =
    runST $ (do a <- thaw initTronMap
                writeArray a initP ' '
                floodFill' a initP)

floodFill' :: (STUArray s (Int, Int) Char) -> (Int, Int) -> ST s (Bool, Int)
floodFill' a p@(x, y) = do
  v <- readArray a p
  (case v of
     'X' -> return $ (False, 0)
     '#' -> return $ (False, 0)
     '2' -> return $ (True, 0)
     '1' -> return $ (True, 0)
     ' ' ->
         (do
           writeArray a p 'X'
           n <- floodFill' a (x, y-1)
           s <- floodFill' a (x, y+1)
           e <- floodFill' a (x+1, y)
           w <- floodFill' a (x-1, y)
           return (or $ map fst [n,s,e,w], sum $ map snd [n,s,e,w]))
     other -> error ("floodFill' encountered " ++ show other))

eitherLefts   :: [Either a b] -> [a]
eitherLefts x = [a | Left a <- x]

eitherRights   :: [Either a b] -> [b]
eitherRights x = [a | Right a <- x]



-- astar = undefined



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

-- oneWayMove :: TronMap -> (Int, Int) -> Move -> Bool
-- oneWayMove tronMap p m =
--     case possibleMoves tronMap p' of
--       [] -> True
--       [m'] ->
--           case possibleMoves tronMap (updatePos p' m') of
--             [] -> True
--             [m''] -> maybeTrace ("oneWayMove " ++ show m'') True
--             _otherwise -> False
--       _otherwise -> False
--     where
--       p' = updatePos p m

updatePos :: (Int, Int) -> Move -> (Int, Int)
updatePos (x, y) North = (x, y-1)
updatePos (x, y) South = (x, y+1)
updatePos (x, y) East = (x+1, y)
updatePos (x, y) West = (x-1, y)

crashed :: TronMap -> (Int, Int) -> Bool
crashed tronMap p = (tronMap ! p) /= showSpot Blank



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
    updateResult :: a -> Float -> Float
    exploratoryConstant :: a -> Float


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
                  uctZipperUp
                  parentLoc (updateResult state 1.0) True
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
            result'' = updateResult state maxResult
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

      state = nodeState label

      result' = updateResult state result



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
          ((exploratoryConstant (nodeState node))
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
