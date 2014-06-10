module Main where

import GOL
import GameLogic

main :: IO ()
main = do
        runSimulation $ setupGrid 100 75
