module Main where

import GOL
import GameLogic

main :: IO ()
main = runSimulation $ setupGrid 100 75
