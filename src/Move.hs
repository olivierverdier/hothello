module Move where

import Protolude


import Player (Player, Cell)
import Coordinate (Coordinate)
import Board (Board, swapAt, gatherAllEnemyCells, putAt, getCell)



swapCells :: [Coordinate] -> Board -> Board
swapCells cells board = foldr swapAt board cells

-- rules

allowFromGathered :: [Coordinate] -> Bool
allowFromGathered = not . null

allowFromCell :: Cell -> Bool
allowFromCell = isNothing

moveGather :: (Monad m) => Player -> Coordinate -> StateT Board m [Coordinate]
moveGather me coord = do
  board <- get
  let cells = gatherAllEnemyCells board me coord
  put (swapCells cells board)
  return cells

moveCheck :: (Monad m) => Player -> Coordinate -> StateT Board m Cell
moveCheck me coord = do
  board <- get
  put (putAt me coord board)
  return (getCell board coord)

move :: (Monad m) => Player -> Coordinate -> StateT Board m Bool
move me coord = do
  cells <- moveGather me coord
  pos <- moveCheck me coord
  return (allowFromCell pos && allowFromGathered cells)

