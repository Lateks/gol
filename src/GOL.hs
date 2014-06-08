module GOL where

import Debug.Trace
import Data.Array.IArray
import Data.Array.Unboxed

data GameState = GameState {
    world :: World,
    mode :: GOLState,
    timeSinceLastUpdate :: Float,
    lastIteration :: Integer
}

data GOLState = Running | Paused | Stabilized

evolveState       s   = s { world = evolve $ world s }
setIterationTime  t s = s { lastIteration = t }
resetTimeDelta    s   = s { timeSinceLastUpdate = 0 }
increaseTimeDelta t s = s { timeSinceLastUpdate = timeSinceLastUpdate s + t }

pause :: GameState -> GameState
pause s = s { mode = Paused }

play :: GameState -> GameState
play s = s { mode = Running }

stop :: GameState -> GameState
stop s = s { mode = Stabilized }

paused :: GameState -> Bool
paused s = case mode s of
                Paused -> True
                _      -> False

type Grid = UArray (Int, Int) Bool

data World = World {
    width :: Int,
    height :: Int,
    grid :: Grid
} deriving (Show)

setupGrid :: Int -> Int -> World
setupGrid x y = let cells = replicate (x*y) False
                    grid = listArray ((0, 0), (x-1, y-1)) cells
                    in World { width = x, height = y, grid = grid }

neighbourhood :: World -> (Int, Int) -> [Bool]
neighbourhood world (x, y) = map (index $ grid world) neighbourPositions
    where neighbourPositions = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], (a,b) /= (x,y)]
          index grid position = grid ! torusIndex position
          torusIndex (x, y) = (((w + x) `mod` w), ((h + y) `mod` h))
          w = width world
          h = height world

liveNeighbours :: World -> (Int, Int) -> Int
liveNeighbours grid (position) = length . filter (== True) $ neighbourhood grid position

liveOrDie :: World -> (Int, Int) -> Bool
liveOrDie world position = case liveNeighbours world position of
                               2 -> (grid world) ! position
                               3 -> True
                               _ -> False

evolve :: World -> World
evolve g = g { grid = listArray ((0, 0), ((width g) - 1, (height g) - 1)) newCells }
    where positions = indices $ grid g
          newCells = map (liveOrDie g) positions

setCellAt :: GameState -> Int -> Int -> Bool -> GameState
setCellAt state x y alive = let worldGrid = world state
                                newWorld = worldGrid { grid = (grid worldGrid) // [((x, y), alive)] }
                                in state { world = newWorld }
