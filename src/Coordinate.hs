module Coordinate where

import Protolude

import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Positive (Positive))

data Coordinate = MkCoordinate Int Int deriving (Eq, Ord, Show)

instance Arbitrary Coordinate where
  arbitrary = do
    Positive x <- arbitrary
    Positive y <- arbitrary
    return (MkCoordinate x y)

data Vector = MkVector Int Int deriving (Show)


class Action a where
  plus :: Vector -> a -> a

instance Action Coordinate where
  plus (MkVector vi vj) (MkCoordinate i j) = MkCoordinate (i+vi) (j+vj)

instance Action Vector where
  plus (MkVector i j) (MkVector vi vj) = MkVector (i+vi) (j+vj)

data Direction = N | S | E | W | NW | NE | SE | SW deriving Enum


getVector :: Direction -> Vector
getVector N = MkVector (-1)  0
getVector S = MkVector 1  0
getVector E = MkVector 0  1
getVector W = MkVector 0  (-1)
getVector NW = plus (getVector N) (getVector W)
getVector NE = plus (getVector N) (getVector E)
getVector SE = plus (getVector S) (getVector E)
getVector SW = plus (getVector S) (getVector W)

coordsInDir :: Coordinate -> Direction -> [Coordinate]
coordsInDir coord dir = iterate (plus vec) coord where
  vec = getVector dir

allDirections :: [Direction]
allDirections = enumFrom N
