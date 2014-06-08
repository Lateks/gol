module Main where

import GOL
import Graphics

main :: IO ()
main = do
        runVisualization $ setupGrid 100 75
