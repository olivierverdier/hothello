module Main where

import Player (Player(Black))
import Game
import Board
import Control.Monad.State.Lazy (get, put, StateT, liftIO, runStateT, lift)
import Input


reaction :: (Monad m) => Maybe Input -> StateT Player (StateT Board m) String
reaction Nothing = return "??"
reaction (Just (Left _)) = do
  _ <- switchPlayer
  return "pass"
reaction (Just (Right c)) = do
  currentPlayer <- get
  currentBoard <- lift get
  legal <- play c
  board <- lift get
  if legal
    then
      return (printStdBoard board)
    else do
      put currentPlayer
      lift (put currentBoard) -- return to previous state
      return "illegal move"


getMove :: IO (Maybe Input)
getMove = do
  -- hFlush stdout
  l <- getLine
  return (parseInput l)

prompt :: Game -> IO()
prompt (MkGame board player) =
  do
    print player
    putStrLn (printStdBoard board)

gameSession :: StateT Player (StateT Board IO) ()
gameSession = do
  player <- get
  board <- lift get
  let game = MkGame board player
  _ <- liftIO (prompt game)
  mi <- liftIO getMove
  s <- reaction mi
  _ <- liftIO (putStrLn s)
  gameSession

main :: IO ()
main = do
  _ <- runStateT (runStateT gameSession Black) startBoard
  return ()

