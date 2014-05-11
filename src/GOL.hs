module GOL where

import qualified TorusZip as TZ
import qualified CyclicZip as CZ
import Control.Monad (liftM2)
import Control.Comonad

data GameState = GameState {
    world :: Grid,
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

data Grid = Grid {
    width :: Int,
    height :: Int,
    grid :: TZ.TorusZipper Bool
} deriving (Show)

setupGrid :: Int -> Int -> Grid
setupGrid x y = let row = replicate x False
                    rowZipper = CZ.CZ row
                    grid = CZ.CZ $ replicate y rowZipper
                    in Grid { width = x, height = y, grid = TZ.TZ grid }

randomizeGrid :: Int -> Int -> Grid
randomizeGrid x y = let row1 = CZ.CZ . take x $ cycle [False, True]
                        row2 = CZ.CZ . take x $ cycle [True, False]
                        rows = CZ.CZ . take y $ cycle [row1, row2]
                        in Grid { width = x, height = y, grid = TZ.TZ rows }

neighbourhood :: [TZ.TorusZipper a -> TZ.TorusZipper a]
neighbourhood = horizontal ++ vertical ++ liftM2 (.) horizontal vertical
    where horizontal = [TZ.left, TZ.right]
          vertical   = [TZ.up,   TZ.down ]

neighbourCells :: TZ.TorusZipper Bool -> [Bool]
neighbourCells grid = map (\d -> extract $ d grid) neighbourhood

liveNeighbours :: TZ.TorusZipper Bool -> Int
liveNeighbours grid = length . filter (== True) $ neighbourCells grid

liveOrDie :: TZ.TorusZipper Bool -> Bool
liveOrDie tz = case liveNeighbours tz of
                    2 -> extract tz
                    3 -> True
                    _ -> False

evolve :: Grid -> Grid
evolve g = g { grid = extend liveOrDie $ grid g }
