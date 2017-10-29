module Board where

import Coordinate
import Player

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

type Board = Map.Map Coordinate Player



emptyBoard :: Board
emptyBoard = Map.empty

startBoard :: Board
startBoard = Map.fromList [
  (MakeCoordinate 4 4, Black),
  (MakeCoordinate 4 5, White),
  (MakeCoordinate 5 4, White),
  (MakeCoordinate 5 5, Black)]

getRow :: Int -- row size
  -> Int -- row
  -> Board
  -> [Cell]
getRow size r board = do
  i <- [1..size]
  return (Map.lookup (MakeCoordinate r i) board)

printRow :: [Cell] -> String
printRow l = intercalate " " cellStrings
  where cellStrings = fmap printCell l

printBoard :: Coordinate -> Board -> String
printBoard (MakeCoordinate m n) board = intercalate "\n" rows
  where
    rows = do
      rowNb <- [1..m]
      let row = getRow n rowNb board
      return (printRow row)

printStdBoard :: Board -> String
printStdBoard = printBoard (MakeCoordinate 8 8)

isSamePlayerAs :: Player -> Cell -> Bool
isSamePlayerAs p (Just p') = p == p'
isSamePlayerAs _ Nothing = False

hasPlayerAt :: Board -> Player -> Coordinate -> Bool
hasPlayerAt board player coordinate = isSamePlayerAs player (Map.lookup coordinate board)


enemyCellsUntilMe :: Board -> Player -> [Coordinate] ->  [Coordinate]
enemyCellsUntilMe board me coords = getResult (reverse bothEnemy) where
  playerList = fmap (`Map.lookup` board) coords
  both = zip coords (tail playerList)
  enemy = switch me
  bothEnemy = takeWhile ((hasPlayerAt board enemy) . fst) both
  getResult [] = []
  getResult l@(x:_) = if isSamePlayerAs me (snd x)
            then fmap fst l
            else []

gatherAllEnemyCells :: Board -> Player -> Coordinate -> [Coordinate]
gatherAllEnemyCells board player coord = concat cells where
  cells = do
    dir <- enumFrom N
    let vec = getVector dir
    let coords = tail (iterate (plus vec) coord)
    return (enemyCellsUntilMe board player coords)



swap :: Coordinate -> Board -> Board
swap coord board = newBoard mPlayer where
  cell = Map.lookup coord board
  mPlayer = fmap switch cell
  newBoard Nothing = board
  newBoard (Just p) = Map.insert coord p board

playAt :: Player -> Coordinate -> Board -> Board
playAt me coord = Map.insert coord me
