{-# LANGUAGE OverloadedStrings #-}
module Player where

import Protolude


import Test.Tasty.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)

data Player =  Black | White deriving (Eq, Bounded, Enum, Show)

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

switch :: Player -> Player
switch Black = White
switch White = Black


printPlayer :: Player -> Text
printPlayer Black = "●"
printPlayer White = "○"

type Cell = Maybe Player

printCell :: Cell -> Text
printCell Nothing = "."
printCell (Just x) = printPlayer x
