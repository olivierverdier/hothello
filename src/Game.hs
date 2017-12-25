module Game where

import Lib (stateMove)
import Board (Board, startBoard)
import Player (Player(Black), switch)
import Coordinate (Coordinate)
import Control.Monad.State.Lazy 

data Game = MkGame { getBoard :: Board, getPlayer :: Player } deriving Show

startGame :: Game
startGame = MkGame startBoard Black

play :: (Monad m) => Coordinate -> StateT Player (StateT Board m) Bool
play coord  = do
  player <- get
  legal <- lift (stateMove player coord)
  put player
  _ <- switchPlayer
  return legal

switchPlayer :: (Monad m) => StateT Player m ()
switchPlayer = modify switch
