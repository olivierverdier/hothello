module Coordinate where

data Coordinate = MakeCoordinate Int Int deriving (Eq, Ord, Show)
data Vector = MakeVector Int Int deriving (Show)

class Action a where
  plus :: Vector -> a -> a

instance Action Coordinate where
  plus (MakeVector vi vj) (MakeCoordinate i j) = MakeCoordinate (i+vi) (j+vj)

instance Action Vector where
  plus (MakeVector i j) (MakeVector vi vj) = MakeVector (i+vi) (j+vj)

data Direction = N | S | E | W | NW | NE | SE | SW deriving Enum


getVector :: Direction -> Vector
getVector N = MakeVector (-1)  0
getVector S = MakeVector 1  0
getVector E = MakeVector 0  1
getVector W = MakeVector 0  (-1)
getVector NW = plus (getVector N) (getVector W)
getVector NE = plus (getVector N) (getVector E)
getVector SE = plus (getVector S) (getVector E)
getVector SW = plus (getVector S) (getVector W)

