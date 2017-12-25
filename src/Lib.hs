module Lib where

import Data.Maybe (isNothing)
import Control.Monad.State.Lazy (StateT, get, put, runState)

import Player (Player, Cell)
import Coordinate (Coordinate)
import Board (Board, swap, gatherAllEnemyCells, putAt, getCell)



swapCells :: [Coordinate] -> Board -> Board
swapCells cells board = foldr swap board cells

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

stateMove :: (Monad m) => Player -> Coordinate -> StateT Board m Bool
stateMove me coord = do
  cells <- moveGather me coord
  pos <- moveCheck me coord
  return (allowFromCell pos && allowFromGathered cells)

onlyIf :: Bool -> a -> Maybe a
onlyIf f x = if f then Just x else Nothing

tryMove :: Player -> Coordinate -> Board -> Maybe Board
tryMove me coord board = onlyIf legal newBoard where
  (legal, newBoard) = runState (stateMove me coord) board



someFunc :: IO ()
someFunc = putStrLn "someFunc"
