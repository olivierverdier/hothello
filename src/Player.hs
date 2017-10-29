module Player where

data Player =  Black | White deriving Eq

switch :: Player -> Player
switch Black = White
switch White = Black



instance Show Player where
  show Black = "●"
  show White = "○"

type Cell = Maybe Player

printCell :: Cell -> String
printCell Nothing = "."
printCell (Just x) = show x
