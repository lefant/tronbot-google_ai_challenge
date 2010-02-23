import System.IO --(hSetBuffering, stdin, stdout, LineBuffering)
import System.Random (StdGen, newStdGen, randomR, split)
import Data.List (foldl', findIndex, sort)
import Debug.Trace (trace)

import Data.Maybe (fromJust)
import Data.Tree (Tree(..))
import Data.Tree.UCT
-- import Data.Array.Diff
import Data.Array.IArray
import Data.Array.Unboxed

-- type TronMap = DiffArray (Int, Int) Spot
type TronMap = UArray (Int, Int) Char

setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

main = do
  rGen <- newStdGen
  playBot uctBot rGen

                                     
playBot :: (GameState -> StdGen -> (Move, StdGen)) -> StdGen -> IO ()
playBot bot starting = do
    setBuffers
    interact ((playTurns bot starting) . lines)

readInt :: String -> Int
readInt a = read a

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

showSpot Wall = '#'
showSpot Blank = ' '
showSpot Player = '1'
showSpot Enemy = '2'


makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns bot pastValue [] = ""
playTurns bot pastValue str =
    -- trace ("playTurns " ++ (show move))
    (makeMove move) ++ "\n" ++ playTurns bot history (drop (h+1) str)
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
              (playerCrashed state) || (enemyCrashed state)

    finalResult state =
        -- trace ("finalResult "
        --        ++ show (((playerCrashed state), (enemyCrashed state)),who,result)
        --        ++ show (moveHistory state)
        --        -- ++ showMap (getTronMap state)
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
                (False, True) ->
                    case who of
                      Player -> 1.0
                      Enemy -> 0.0
                (False, False) ->
                    error "finalResult called when neither player crashed"
          who = lastToMove state

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
          who = toMove state


finalResult2 state who =
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
                (False, True) ->
                    case who of
                      Player -> 1.0
                      Enemy -> 0.0
                (False, False) ->
                    error "finalResult called when neither player crashed"



uctBot :: GameState -> StdGen -> (Move, StdGen)
uctBot state rGen =
    case possibleMoves tronMap (playerPos state) of
      [] ->
          trace ("uctBot no possible moves, go North")
           (North, rGen)
      [onlyMove] ->
          trace ("uctBot only one move: " ++ show onlyMove)
           (onlyMove, rGen)
      _otherwise -> (move, g')
    where
      move =
          case pv of
            [] ->
                trace ("uctBot uct found no moves, go North")
                North
            pv' ->
                trace ("uctBot\n"
                       ++ show alternateFirstMoves ++ "\n"
                       -- ++ show pv' ++ "\n"
                      )
                snd $ last $ moveHistory $ nodeState $
                    head $ pv'

      alternateFirstMoves =
          map rootLabel $ reverse $ sort $ subForest t

      pv = principalVariation t
      t = uct state 5000 g
      
      tronMap = getTronMap state
      (g, g') = split rGen



runOneRandom :: GameState -> StdGen -> Float
runOneRandom initState initGen =
    -- trace ("runOneRandom " ++ showMap (getTronMap initState))
    run initState initGen 0
    where
      run :: GameState -> StdGen -> Int -> Float
      run _ _ 1000 =
          trace ("run returning after 1000 iterations")
          0.5
      run state rGen runCount =
          if isTerminalNode state'
          then
              -- trace ("run is terminal")
              finalResult2 state' who
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
      (i, rGen') = randomR (0, ((length as) - 1)) rGen


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
                (playerCrashed state) || bothCrashed
      enemyCrashed' =
          case who of
            Player ->
                False
            Enemy ->
                crashed tronMap enemyPos' || bothCrashed

      bothCrashed = playerPos' == enemyPos'

      playerPos' =
          case who of
            Player -> updatePos (playerPos state) move
            Enemy -> (playerPos state)
      enemyPos' =
          case who of
            Player -> (enemyPos state)
            Enemy -> updatePos (enemyPos state) move

      tronMap' = tronMap // updates
      updates = [(position, (showSpot Wall)),
                 (position',(showSpot who))]

      position =
          case who of
            Player -> (playerPos state)
            Enemy -> (enemyPos state)
      position' =
          case who of
            Player -> playerPos'
            Enemy -> enemyPos'


      moveHistory' = (moveHistory state) ++ [(who, move)]
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
          (tronMap ! (updatePos position move)) /= (showSpot Wall)

updatePos (x, y) North = (x, y-1)
updatePos (x, y) South = (x, y+1)
updatePos (x, y) East = (x+1, y)
updatePos (x, y) West = (x-1, y)

crashed :: TronMap -> (Int, Int) -> Bool
crashed tronMap p =
    -- trace ("crashed: " ++ show (p, result))
    result
    where
      result = (tronMap ! p) /= (showSpot Blank)

showMap :: TronMap -> String
showMap tronMap =
    "\n" ++
    (show $ assocs tronMap)
    ++ "\n"

completeRound state = Player == toMove state

moverPos state =
    case toMove state of
      Player -> playerPos state
      Enemy -> enemyPos state

lastToMove state =
    case toMove state of
      Player -> Enemy
      Enemy -> Player
