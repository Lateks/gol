module UI where

import Graphics.UI.SDL as SDL
import Control.Monad (forM_)
import Data.Array.IArray
import GameState
import GOL

cellWidth, cellPadding, paddedCellWidth :: Int
cellPadding = 2
cellWidth = 6
paddedCellWidth = cellWidth + cellPadding

background, liveCell, deadCell :: Pixel
background = Pixel 0x0B486B
liveCell = Pixel 0xCFF09E
deadCell = Pixel 0x3B8686

type ShouldQuit = Bool
type WorldChanged = Bool

drawWorld :: Surface -> WindowConfig -> World -> IO ()
drawWorld surface window worldGrid = do
    fillRect surface (Just (Rect 0 0 (winWidth window) (winHeight window))) background

    let cells = grid worldGrid
        indexedCells = zip (indices cells) (elems cells)

    forM_ indexedCells $ \((x, y), cell) ->
        let color = if cell then liveCell else deadCell
            cellDimensions = Rect (x * paddedCellWidth) (y * paddedCellWidth) cellWidth cellWidth
            in fillRect surface (Just cellDimensions) color

    SDL.flip surface

loopEvents :: GameState -> IO (ShouldQuit, WorldChanged, GameState)
loopEvents = eventLoop False
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

setCellAtPixel :: Integral a => GameState -> a -> a -> Bool -> GameState
setCellAtPixel state xPixel yPixel = setCellAt state x y
    where x = toGridIndex xPixel
          y = toGridIndex yPixel
          toGridIndex pixel = fromIntegral pixel `div` paddedCellWidth
