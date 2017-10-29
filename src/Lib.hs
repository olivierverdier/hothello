module Lib where

import Data.Maybe (isNothing)


import Player
import Coordinate
import Board
import qualified Data.Map.Strict as Map






swapGathered :: [Coordinate] -> Board -> Board
swapGathered gathered board = foldr swap board gathered

-- rules

allowFromGathered :: [Coordinate] -> Bool
allowFromGathered = not . null

allowFromCell :: Cell -> Bool
allowFromCell = isNothing

addPieceAt :: Player -> Coordinate -> Board -> Board
addPieceAt me coord = Map.insert coord me

tryMove :: Player -> Coordinate -> Board -> Maybe Board
tryMove me coord board = result where
  gathered = gatherAllEnemyCells board me coord
  current = Map.lookup coord board
  legal = allowFromCell current && allowFromGathered gathered
  newBoard = addPieceAt me coord (swapGathered gathered board)
  result = if legal then Just newBoard else Nothing




someFunc :: IO ()
someFunc = putStrLn "someFunc"
