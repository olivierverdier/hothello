module Lib
    ( someFunc
    ) where

import Data.Maybe (isJust)


import Player
import Coordinate
import Board
import qualified Data.Map.Strict as Map





-- play :: Board -> Coordinate -> Board




tryMove :: Player -> Coordinate -> Board -> Maybe Board
tryMove me coord board = result where
  gathered = gatherAllEnemyCells board me coord
  illegalMove = null gathered
  swapped = foldr swap board gathered
  newBoard = Map.insert coord me swapped
  result = if illegalMove then Nothing else Just newBoard

move :: Player -> Coordinate -> Board -> Maybe Board
move me coord board = result where
  illegalMove = isJust (Map.lookup coord board)
  result = if illegalMove then Nothing else tryMove me coord board



 





someFunc :: IO ()
someFunc = putStrLn "someFunc"
