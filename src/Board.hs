{-# LANGUAGE OverloadedStrings #-}

module Board where

import Protolude hiding (intercalate)

import qualified Data.Map.Strict as Map
import Data.Text (intercalate)

import Coordinate (Coordinate(MakeCoordinate), coordsInDir, allDirections)
import Player (Player(Black, White), Cell, printCell, switch)


type Board = Map.Map Coordinate Player

getCell :: Board -> Coordinate -> Cell
getCell board coord = Map.lookup coord board

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

printRow :: [Cell] -> Text
printRow l = intercalate " " cellStrings
  where cellStrings = fmap printCell l

printBoard :: Coordinate -> Board -> Text
printBoard (MakeCoordinate m n) board = intercalate "\n" rows
  where
    rows = do
      rowNb <- [1..m]
      let row = getRow n rowNb board
      return (printRow row)

printStdBoard :: Board -> Text
printStdBoard = printBoard (MakeCoordinate 8 8)

isSamePlayerAs :: Player -> Cell -> Bool
isSamePlayerAs p (Just p') = p == p'
isSamePlayerAs _ Nothing = False

hasPlayerAt :: Board -> Player -> Coordinate -> Bool
hasPlayerAt board player coordinate = isSamePlayerAs player (Map.lookup coordinate board)


enemyCellsUntilMe :: Board -> Player -> [Coordinate] ->  [Coordinate]
enemyCellsUntilMe board me coords = getResult (reverse bothEnemy) where
  playerList = fmap (`Map.lookup` board) coords
  both = zip coords (tailSafe playerList)
  enemy = switch me
  bothEnemy = takeWhile (hasPlayerAt board enemy . fst) both
  getResult [] = []
  getResult l@(x:_) = if isSamePlayerAs me (snd x)
            then fmap fst l
            else []

gatherAllEnemyCells :: Board -> Player -> Coordinate -> [Coordinate]
gatherAllEnemyCells board player coord = concat cells where
  cells = do
    dir <- allDirections
    let coords = tailSafe (coordsInDir coord dir)
    return (enemyCellsUntilMe board player coords)



swapAt :: Coordinate -> Board -> Board
swapAt coord board = newBoard mPlayer where
  cell = Map.lookup coord board
  mPlayer = fmap switch cell
  newBoard Nothing = board
  newBoard (Just p) = Map.insert coord p board

putAt :: Player -> Coordinate -> Board -> Board
putAt me coord = Map.insert coord me

putWhite :: Coordinate -> Board -> Board
putWhite = putAt White

putBlack :: Coordinate -> Board -> Board
putBlack = putAt Black

putColAt :: Player -> Int -> [Int] -> Board -> Board
putColAt me col rows board = foldr (\ r b -> putAt me (MakeCoordinate r col) b) board rows
