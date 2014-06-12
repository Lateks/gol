module GameState where

import Data.Array.IArray
import GOL

stepIntervalChange = 50.0

data GameState = GameState {
    world :: World,
    mode :: GOLState,
    timeSinceLastUpdate :: Float,
    lastIteration :: Integer,
    window :: WindowConfig,
    stepInterval :: Float
}

data WindowConfig = WindowConfig {
    winWidth :: Int,
    winHeight :: Int
}

data GOLState = Running | Paused

evolveState       s   = s { world = evolve $ world s }
setIterationTime  t s = s { lastIteration = t }
resetTimeDelta    s   = s { timeSinceLastUpdate = 0 }
increaseTimeDelta t s = s { timeSinceLastUpdate = timeSinceLastUpdate s + t }
increaseStepInterval s = s { stepInterval = stepInterval s + stepIntervalChange }
decreaseStepInterval s =
    if stepInterval s == 0.0
        then s
        else s { stepInterval = stepInterval s - stepIntervalChange }

pause :: GameState -> GameState
pause s = s { mode = Paused }

play :: GameState -> GameState
play s = s { mode = Running }

paused :: GameState -> Bool
paused s = case mode s of
                Paused -> True
                _      -> False

stepSimulation :: GameState -> Integer -> (Bool, GameState)
stepSimulation state time =
    if paused state
       then (False, setIterationTime time state)
       else let deltaMs = psToMs $ time - lastIteration state
                st = setIterationTime time $ increaseTimeDelta deltaMs state
                in if timeSinceLastUpdate st >= stepInterval state
                      then (True, resetTimeDelta $ evolveState st)
                      else (False, st)
    where psToMs ps = fromIntegral ps / 1000000000

setCellAt :: GameState -> Int -> Int -> Bool -> GameState
setCellAt state x y alive = let worldGrid = world state
                                newWorld = worldGrid { grid = grid worldGrid // [((x, y), alive)] }
                                in state { world = newWorld }
