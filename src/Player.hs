module Player where

import Test.Tasty.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)

data Player =  Black | White deriving (Eq, Bounded, Enum, Show)

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

switch :: Player -> Player
switch Black = White
switch White = Black


printPlayer :: Player -> String
printPlayer Black = "●"
printPlayer White = "○"

type Cell = Maybe Player

printCell :: Cell -> String
printCell Nothing = "."
printCell (Just x) = printPlayer x
