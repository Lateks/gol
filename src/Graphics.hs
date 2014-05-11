{-# LANGUAGE BangPatterns #-}

module Graphics where

import Debug.Trace
import System.CPUTime (getCPUTime)
import Graphics.UI.SDL as SDL
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless, when, forM_)
import Control.Comonad (extract)
import GOL
import qualified TorusZip as TZ

windowHeight, windowWidth :: Int
windowHeight = 600
windowWidth  = 800

indigo, background, liveCell, deadCell :: Pixel
indigo = Pixel 0x2E0854
background = Pixel 0x0B486B
liveCell = Pixel 0xCFF09E
deadCell = Pixel 0x3B8686

runVisualization :: Grid -> IO ()
runVisualization grid = do
    SDL.init [InitEverything]
    setCaption "Game of Life" ""
    window <- setVideoMode windowWidth windowHeight 32 [HWSurface, DoubleBuf]
    initialState <- initGame grid

    drawWorld window (world initialState)
    runStateT (mainLoop window) initialState

    SDL.quit

initGame :: Grid -> IO GameState
initGame grid = do
    !time <- getCPUTime
    return GameState {
        world = grid,
        mode = Paused,
        timeSinceLastUpdate = 0.0,
        lastIteration = time
    }

drawWorld :: Surface -> Grid -> IO ()
drawWorld surface worldGrid = do
    fillRect surface (Just (Rect 0 0 windowWidth windowHeight)) background

    let lines = TZ.toList $ grid worldGrid
        indexedLines = zip [0..] (map (zip [0..]) lines)

    forM_ indexedLines $ \(y, line) ->
        forM_ line $ \(x, cell) ->
            let color = case cell of
                            True -> liveCell
                            False -> deadCell
                in fillRect surface (Just (Rect (x * 8) (y * 8) 6 6)) color

    SDL.flip surface

psToMs :: Integer -> Float
psToMs ps = fromIntegral ps / 1000000000

stepSimulation :: GameState -> Integer -> (Bool, GameState)
stepSimulation state time =
    if paused state
       then (False, setIterationTime time state)
       else let deltaMs = psToMs $ time - lastIteration state
                st = setIterationTime time $ increaseTimeDelta deltaMs state
                in if timeSinceLastUpdate st >= 0.5
                      then (True, resetTimeDelta $ evolveState st)
                      else (False, st)

mainLoop :: Surface -> StateT GameState IO ()
mainLoop window = do
    state <- get
    !time <- liftIO getCPUTime
    let (rerender, evolvedState) = stepSimulation state time

    (quit, newState) <- liftIO $ loopEvents evolvedState

    liftIO . (drawWorld window) $ world evolvedState

    put newState
    unless quit $ mainLoop window

loopEvents :: GameState -> IO (Bool, GameState)
loopEvents state = do
    event <- pollEvent
    case event of
        NoEvent  -> return (False, state)
        SDL.Quit -> return (True, state)
        _        -> let newState = handleEvent state event
                        in loopEvents newState

handleEvent :: GameState -> Event -> GameState
handleEvent s@(GameState _ Paused _ _) e = -- TODO: mouse handling
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> play s
        MouseButtonDown x y btn         ->
            let alive = case btn of
                            ButtonLeft -> True
                            _          -> False
                        in setCellAt s (fromIntegral x) (fromIntegral y) alive
        _                               -> s
handleEvent s@(GameState _ Running _ _) e =
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> pause s
        _                               -> s
handleEvent s _ = s

setCellAt :: GameState -> Int -> Int -> Bool -> GameState
setCellAt state x y alive = let (Grid w h z) = world state
                                cellIndex = (x `quot` 8, y `quot` 8)
                                moveRight = foldr (.) id $ replicate (fst cellIndex) TZ.right
                                moveLeft  = foldr (.) id $ replicate (fst cellIndex) TZ.left
                                moveDown  = foldr (.) id $ replicate (snd cellIndex) TZ.down
                                moveUp    = foldr (.) id $ replicate (snd cellIndex) TZ.up
                                g         = moveUp . moveLeft . (TZ.replace alive) .
                                            moveRight . moveDown $ z
                                newGrid   = Grid w h g
                                in state { world = newGrid }
