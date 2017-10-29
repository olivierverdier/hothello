module Player where

import Test.Tasty.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)

data Player =  Black | White deriving (Eq, Bounded, Enum)

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

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
