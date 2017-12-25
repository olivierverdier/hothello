module Util where

import Player(Player)
import Coordinate(Coordinate)
import Board(Board)

import Move(stateMove)

import Control.Monad.State.Lazy (runState)

onlyIf :: Bool -> a -> Maybe a
onlyIf f x = if f then Just x else Nothing

tryMove :: Player -> Coordinate -> Board -> Maybe Board
tryMove me coord board = onlyIf legal newBoard where
  (legal, newBoard) = runState (stateMove me coord) board
