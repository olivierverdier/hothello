module Game where

import Move (move)
import Board (Board, startBoard)
import Player (Player(Black), switch)
import Coordinate (Coordinate)
import Control.Monad.State.Lazy (StateT, get, lift, put, modify)

data Game = MkGame { getBoard :: Board, getPlayer :: Player } deriving Show

startGame :: Game
startGame = MkGame startBoard Black

play :: (Monad m) => Coordinate -> StateT Player (StateT Board m) Bool
play coord  = do
  player <- get
  legal <- lift (move player coord)
  put player
  _ <- switchPlayer
  return legal

switchPlayer :: (Monad m) => StateT Player m ()
switchPlayer = modify switch

putGame :: (Monad m) => Game -> StateT Player (StateT Board m) ()
putGame (MkGame board player) = do
  put player
  lift (put board)


getGame :: (Monad m) => StateT Player (StateT Board m) Game
getGame = do
  player <- get
  board <- lift get
  return (MkGame board player)
