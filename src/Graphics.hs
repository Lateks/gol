{-# LANGUAGE BangPatterns #-}

module Graphics where

import Debug.Trace
import System.CPUTime (getCPUTime)
import Graphics.UI.SDL as SDL
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless, when, forM_)
import GOL
import GameState
import Data.Array.IArray

cellWidth, cellPadding, paddedCellWidth :: Int
cellPadding = 2
cellWidth = 6
paddedCellWidth = cellWidth + cellPadding

background, liveCell, deadCell :: Pixel
background = Pixel 0x0B486B
liveCell = Pixel 0xCFF09E
deadCell = Pixel 0x3B8686

type ShouldQuit = Bool
type Changed = Bool

runVisualization :: World -> IO ()
runVisualization gameWorld = do
    SDL.init [InitEverything]
    setCaption "Game of Life" ""

    let window = WindowConfig {
            winWidth = (gridWidth gameWorld) * paddedCellWidth,
            winHeight = (gridHeight gameWorld) * paddedCellWidth
        }
    sdlWindow <- setVideoMode (winWidth window) (winHeight window) 32 [HWSurface, DoubleBuf]

    initialState <- initGame gameWorld window

    drawWorld sdlWindow window (world initialState)
    runStateT (mainLoop sdlWindow) initialState

    SDL.quit

initGame :: World -> WindowConfig -> IO GameState
initGame grid windowConfig = do
    !time <- getCPUTime
    return GameState {
        world = grid,
        mode = Paused,
        timeSinceLastUpdate = 0.0,
        lastIteration = time,
        window = windowConfig,
        stepInterval = 100.0
    }

mainLoop :: Surface -> StateT GameState IO ()
mainLoop sdlWindow = do
    state <- get
    !time <- liftIO getCPUTime
    let (worldEvolved, evolvedState) = stepSimulation state time

    (quit, worldAltered, newState) <- liftIO $ loopEvents evolvedState

    when (worldEvolved || worldAltered) $
         liftIO $ drawWorld sdlWindow (window newState) (world newState)

    put newState
    unless quit $ mainLoop sdlWindow

drawWorld :: Surface -> WindowConfig -> World -> IO ()
drawWorld surface window worldGrid = do
    fillRect surface (Just (Rect 0 0 (winWidth window) (winHeight window))) background

    let cells = grid worldGrid
        indexedCells = zip (indices cells) (elems cells)

    forM_ indexedCells $ \((x, y), cell) ->
        let color = case cell of
                        True -> liveCell
                        False -> deadCell
            cellDimensions = Rect (x * paddedCellWidth) (y * paddedCellWidth) cellWidth cellWidth
            in fillRect surface (Just cellDimensions) color

    SDL.flip surface

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

loopEvents :: GameState -> IO (ShouldQuit, Changed, GameState)
loopEvents state = eventLoop False state
    where eventLoop changed state = do
            event <- pollEvent
            case event of
                NoEvent  -> return (False, changed, state)
                SDL.Quit -> return (True, changed, state)
                _        -> let newState = handleEvent state event
                                in case newState of
                                       Nothing -> eventLoop changed state
                                       Just s  -> eventLoop True s

handleEvent :: GameState -> Event -> Maybe GameState
handleEvent s@(GameState _ Paused _ _ _ _) e =
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> Just $ play s
        MouseButtonDown x y btn         -> let alive = case btn of
                                                           ButtonLeft -> True
                                                           _          -> False
                                               in Just $ setCellAtPixel s x y alive
        _                               -> Nothing
handleEvent s@(GameState _ Running _ _ _ _) e =
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> Just $ pause s
        KeyDown (Keysym SDLK_PLUS  _ _) -> Just $ decreaseStepInterval s
        KeyDown (Keysym SDLK_MINUS _ _) -> Just $ increaseStepInterval s
        _                               -> Nothing

setCellAtPixel state xPixel yPixel alive = setCellAt state x y alive
    where x = toGridIndex xPixel
          y = toGridIndex yPixel
          toGridIndex pixel = (fromIntegral pixel) `div` paddedCellWidth
