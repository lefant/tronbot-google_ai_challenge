import System.IO --(hSetBuffering, stdin, stdout, LineBuffering)
import System.Random (StdGen,newStdGen,randomR,split)
import Data.List (foldl',findIndex,sort)
import Debug.Trace (trace)

import Data.Maybe (fromJust)
import Data.Tree (Tree(..))
import Data.Tree.UCT

setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

main = do
  rGen <- newStdGen
  playBot uctBot rGen

                                     
playBot :: ([[Spot]] -> a -> (Move, a)) -> a -> IO ()
playBot bot starting = do
    setBuffers
    interact ((playTurns bot starting) . lines)

readInt :: String -> Int
readInt a = read a

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns bot pastValue [] = ""
playTurns bot pastValue str =
    -- trace ("playTurns " ++ (show move))
    (makeMove move) ++ "\n" ++ playTurns bot history (drop (h+1) str)
    where [w,h] = map readInt (words $ head str)
          tronMap = map (map readSpot) (take h (tail str))
	  (move, history) =
              -- trace ("playTurns\n" ++
              --        (unlines $ map (unwords . (map show)) tronMap))
              bot tronMap pastValue



data Spot = Wall | Blank | Player | Enemy deriving Eq
data Move = North | East | South | West deriving (Eq, Show)

instance Show Spot where
    show Wall = "#"
    show Blank = " "
    show Player = "1"
    show Enemy = "2"

data GameState = GameState {
      getTronMap      :: [[Spot]]
     ,moveHistory     :: [(Spot, Move)]
     ,ourRandomGen    :: StdGen
     ,simulCount      :: Int
     ,playerCrashed   :: Bool
     ,enemyCrashed    :: Bool
    }

newGameState tronMap rGen =
    GameState { getTronMap = tronMap,
                ourRandomGen = rGen,
                moveHistory = [],
                playerCrashed = False,
                enemyCrashed = False,
                simulCount = 100 }

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
        -- trace ("finalResult " ++
        --        show (((playerCrashed state), (enemyCrashed state)),who,result))
               -- show moveHistory state) ++
               -- showMap (getTronMap state) ++
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
                   (pos tronMap (toMove state)) of
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
      moveList = possibleMoves tronMap (pos tronMap (toMove state))
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
          state { getTronMap = tronMap',
                  moveHistory = moveHistory',
                  playerCrashed = playerCrashed',
                  enemyCrashed = enemyCrashed' }

      playerCrashed' =
          case who of
            Player ->
                crashed tronMap position'
            Enemy ->
                (playerCrashed state) || bothCrashed
      enemyCrashed' =
          case who of
            Player ->
                False
            Enemy ->
                crashed tronMap position' || bothCrashed

      bothCrashed = position' == pos tronMap Player


      tronMap' = foldl' updateList tronMap updates
      updates = [(position, Wall),
                 (position', who)]

      moveHistory' = (moveHistory state) ++ [(who, move)]

      position' = updatePos position move
      
      -- moves = possibleMoves tronMap pos
      position = pos tronMap who
      tronMap = getTronMap state

      who = toMove state


startingValue = ()

-- player tronMap = (maybe 0 id (findIndex (== Player) (head $ filter (any (== Player)) tronMap)), maybe 0 id (findIndex (any (== Player)) tronMap))

-- enemy tronMap = (maybe 0 id (findIndex (== Enemy) (head $ filter (any (== Enemy)) tronMap)), maybe 0 id (findIndex (any (== Enemy)) tronMap))

pos tronMap who =
    (fromJust (findIndex (== who) (head $ filter (any (== who)) tronMap)), fromJust (findIndex (any (== who)) tronMap))

canMove move (x,y) tronMap
    | move == North	= if y == 0 then False else (Blank == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (Blank == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (Blank == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (Blank == ((tronMap !! y) !! (x-1)))




-- testBot :: [[Spot]] -> a -> (Move, a)
-- testBot tronMap b = (head ((possibleMoves tronMap (pos tronMap Player)) ++ [North]), b)

uctBot :: [[Spot]] -> StdGen -> (Move, StdGen)
uctBot tronMap rGen =
    case possibleMoves tronMap (pos tronMap Player) of
      [] -> (North, rGen)
      [onlyMove] -> (onlyMove, rGen)
      _otherwise -> (move, g')
    where
      move =
          case pv of
            [] -> North
            pv' ->
                trace ("uctBot\n"
                       ++ show alternateFirstMoves ++ "\n"
                       ++ show pv' ++ "\n"
                      )
                snd $ last $ moveHistory $ nodeState $
                    head $ pv'

      alternateFirstMoves =
          map rootLabel $ reverse $ sort $ subForest t

      pv = principalVariation t
      t = uct (newGameState tronMap rGen) 1000 g
      (g, g') = split rGen


possibleMoves tronMap pos =
    -- trace ("possibleMoves " ++ show pos ++
    --        showMap tronMap ++ show moves)
    moves
    where
      moves =
          filter
          (\a -> canMove a pos tronMap)
          [North, East, South, West]

updatePos (x, y) North = (x, y-1)
updatePos (x, y) South = (x, y+1)
updatePos (x, y) East = (x+1, y)
updatePos (x, y) West = (x-1, y)

crashed tronMap (x, y) =
    -- trace ("\ncrashed\n\n" ++ (showMap tronMap) ++ (show ((x, y), result)))
    result
    where
      result = Blank /= ((tronMap !! y) !! x)

updateList ls ((x, y), what) =
    init as ++ [lls'] ++ bs
    where
      lls' = init las ++ [what] ++ lbs
      (las, lbs) = splitAt (x+1) lls
      lls = last as
      (as, bs) = splitAt (y+1) ls

showMap tronMap =
    "\n" ++
    (unlines $ map (concat . (map show)) tronMap)
    ++ "\n"

completeRound state = Player == toMove state

toMove state =
    if even $ length $ moveHistory state
    then Player
    else Enemy

lastToMove state =
    case toMove state of
      Player -> Enemy
      Enemy -> Player
