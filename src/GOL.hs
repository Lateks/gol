module GOL where

import Data.Array.IArray
import Data.Array.Unboxed

data World = World {
    gridWidth :: Int,
    gridHeight :: Int,
    grid :: Grid
} deriving (Show)

type Grid = UArray (Int, Int) Bool

setupGrid :: Int -> Int -> World
setupGrid x y = let cells = replicate (x*y) False
                    grid = listArray ((0, 0), (x-1, y-1)) cells
                    in World { gridWidth = x, gridHeight = y, grid = grid }

neighbourhood :: World -> (Int, Int) -> [Bool]
neighbourhood world (x, y) = map (index $ grid world) neighbourPositions
    where neighbourPositions = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], (a,b) /= (x,y)]
          index grid position = grid ! torusIndex position
          torusIndex (x, y) = (((w + x) `mod` w), ((h + y) `mod` h))
          w = gridWidth world
          h = gridHeight world

liveNeighbours :: World -> (Int, Int) -> Int
liveNeighbours grid (position) = length . filter (== True) $ neighbourhood grid position

liveOrDie :: World -> (Int, Int) -> Bool
liveOrDie world position = case liveNeighbours world position of
                               2 -> (grid world) ! position
                               3 -> True
                               _ -> False

evolve :: World -> World
evolve g = g { grid = listArray ((0, 0), ((gridWidth g) - 1, (gridHeight g) - 1)) newCells }
    where positions = indices $ grid g
          newCells = map (liveOrDie g) positions
