{-# LANGUAGE BangPatterns #-}

module GameLogic where

import Graphics.UI.SDL as SDL
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless, when)
import System.CPUTime (getCPUTime)
import GOL
import GameState
import UI

runSimulation :: World -> IO ()
runSimulation gameWorld = do
    SDL.init [InitEverything]
    setCaption "Game of Life" ""

    let window = WindowConfig {
            winWidth = gridWidth gameWorld * paddedCellWidth,
            winHeight = gridHeight gameWorld * paddedCellWidth
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

