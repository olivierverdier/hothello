module Game where

import Lib (stateMove)
import Board (Board, startBoard)
import Player (Player(Black), switch)
import Coordinate (Coordinate)
import Control.Monad.State.Lazy 

data Game = MkGame { getBoard :: Board, getPlayer :: Player } deriving Show

startGame :: Game
startGame = MkGame startBoard Black

play :: Coordinate -> State Game Bool
play coord  = do
  (MkGame board player) <- get
  let (legal, newBoard) = runState (stateMove player coord) board
  put (MkGame newBoard player)
  _ <- switchPlayer
  return legal

switchPlayer :: State Game ()
switchPlayer = do
  (MkGame board player) <- get
  put (MkGame board (switch player))
  return ()
