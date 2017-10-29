module Lib where

import Data.Maybe (isNothing)


import Player
import Coordinate
import Board






swapGathered :: [Coordinate] -> Board -> Board
swapGathered gathered board = foldr swap board gathered

-- rules

allowFromGathered :: [Coordinate] -> Bool
allowFromGathered = not . null

allowFromCell :: Cell -> Bool
allowFromCell = isNothing

tryMove :: Player -> Coordinate -> Board -> Maybe Board
tryMove me coord board = result where
  gathered = gatherAllEnemyCells board me coord
  current = getCell board coord
  legal = allowFromCell current && allowFromGathered gathered
  newBoard = putAt me coord (swapGathered gathered board)
  result = if legal then Just newBoard else Nothing




someFunc :: IO ()
someFunc = putStrLn "someFunc"
