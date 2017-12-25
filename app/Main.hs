module Main where

import Game
import Board
import Control.Monad.State.Lazy (get, gets, put, StateT, liftIO, runStateT)
import Input


reaction :: (Monad m) => Maybe Input -> StateT Game m String
reaction Nothing = return "??"
reaction (Just (Left _)) = do
  _ <- switchPlayer
  return "pass"
reaction (Just (Right c)) = do
  before <- get
  legal <- play c
  board <- gets getBoard
  decision legal board before where
    decision False _ before = do
      put before -- return to previous state
      return "illegal move"
    decision True board _ = return (printStdBoard board)


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

gameSession :: StateT Game IO ()
gameSession = do
  game <- get
  _ <- liftIO (prompt game)
  mi <- liftIO getMove
  s <- reaction mi
  _ <- liftIO (putStrLn s)
  gameSession

main :: IO ()
main = do
  _ <- runStateT gameSession startGame
  return ()

