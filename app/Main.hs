{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Player (Player(Black), printPlayer)
import Game
import Board
import Input


reaction :: (Monad m) => Maybe Input -> StateT Player (StateT Board m) Text
reaction Nothing = return "??"
reaction (Just (Left _)) = do
  _ <- switchPlayer
  return "pass"
reaction (Just (Right c)) = do
  game <- getGame
  legal <- play c
  board <- lift get
  if legal
    then
      return (printStdBoard board)
    else do
      putGame game -- return to previous state
      return "illegal move"


getMove :: IO (Maybe Input)
getMove = do
  -- hFlush stdout
  l <- getLine
  return (parseInput l)

prompt :: Game -> IO()
prompt (MkGame board player) =
  do
    putStrLn (printPlayer player)
    putStrLn (printStdBoard board)

gameTurn :: StateT Player (StateT Board IO) ()
gameTurn = do
  player <- get
  board <- lift get
  let game = MkGame board player
  _ <- liftIO (prompt game)
  mi <- liftIO getMove
  s <- reaction mi
  _ <- liftIO (putStrLn s)
  return ()

gameTurns :: StateT Player (StateT Board IO) ()
gameTurns = sequence_ (fmap (const gameTurn) [(1::Int)..])

main :: IO ()
main = do
  _ <- runStateT (runStateT gameTurns Black) startBoard
  return ()
