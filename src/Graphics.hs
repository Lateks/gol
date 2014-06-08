{-# LANGUAGE BangPatterns #-}

module Graphics where

import Debug.Trace
import System.CPUTime (getCPUTime)
import Graphics.UI.SDL as SDL
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless, when, forM_)
import GOL
import Data.Array.IArray

windowHeight, windowWidth, cellWidth :: Int
windowHeight = 600
windowWidth  = 800
cellWidth = 8

indigo, background, liveCell, deadCell :: Pixel
indigo = Pixel 0x2E0854
background = Pixel 0x0B486B
liveCell = Pixel 0xCFF09E
deadCell = Pixel 0x3B8686

runVisualization :: World -> IO ()
runVisualization grid = do
    SDL.init [InitEverything]
    setCaption "Game of Life" ""
    window <- setVideoMode windowWidth windowHeight 32 [HWSurface, DoubleBuf]
    initialState <- initGame grid

    drawWorld window (world initialState)
    runStateT (mainLoop window) initialState

    SDL.quit

initGame :: World -> IO GameState
initGame grid = do
    !time <- getCPUTime
    return GameState {
        world = grid,
        mode = Paused,
        timeSinceLastUpdate = 0.0,
        lastIteration = time
    }

drawWorld :: Surface -> World -> IO ()
drawWorld surface worldGrid = do
    {-# SCC color_background #-} fillRect surface (Just (Rect 0 0 windowWidth windowHeight)) background

    let cells = grid worldGrid
        indexedCells = zip (indices cells) (elems cells)

    forM_ indexedCells $ \((x, y), cell) ->
        let color = case cell of
                        True -> liveCell
                        False -> deadCell
            in {-# SCC draw_cell #-} fillRect surface (Just (Rect (x * cellWidth) (y * cellWidth) 6 6)) color

    {-# SCC flip_surface #-} SDL.flip surface

psToMs :: Integer -> Float
psToMs ps = fromIntegral ps / 1000000000

stepSimulation :: GameState -> Integer -> (Bool, GameState)
stepSimulation state time =
    if paused state
       then (False, setIterationTime time state)
       else let deltaMs = psToMs $ time - lastIteration state
                st = setIterationTime time $ increaseTimeDelta deltaMs state
                in if timeSinceLastUpdate st >= 100.0
                      then (True, resetTimeDelta $ evolveState st)
                      else (False, st)

mainLoop :: Surface -> StateT GameState IO ()
mainLoop window = {-# SCC mainLoop #-} do
    state <- get
    !time <- liftIO getCPUTime
    let (worldEvolved, evolvedState) = {-# SCC calling_stepSimulation #-} stepSimulation state time

    (quit, userChanged, newState) <- liftIO $ {-# SCC loopEvents #-} loopEvents evolvedState

    when (worldEvolved || userChanged) $
         liftIO . ({-# SCC drawWorld #-} drawWorld window) $ world newState

    put newState
    unless quit $ mainLoop window

loopEvents :: GameState -> IO (Bool, Bool, GameState)
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
handleEvent s@(GameState _ Paused _ _) e = -- TODO: mouse handling
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> trace "Running" $ Just (play s)
        MouseButtonDown x y btn         -> let alive = case btn of
                                                           ButtonLeft -> True
                                                           _          -> False
                                               in Just $ setCell s x y alive
        _                               -> Nothing
handleEvent s@(GameState _ Running _ _) e =
    case e of
        KeyDown (Keysym SDLK_SPACE _ _) -> trace "Paused" $ Just (pause s)
        _                               -> Nothing
handleEvent s _ = Nothing

setCell state xPixel yPixel alive = setCellAt state x y alive
    where x = toGridIndex xPixel
          y = toGridIndex yPixel
          toGridIndex pixel = (fromIntegral pixel) `div` cellWidth
