import Test.Tasty
import Test.Tasty.HUnit

import Lib 
import Board
import Player
import Coordinate


board6 :: Board
board6 = putColAt Black 1 [1..3] $ putColAt White 2 [1..3] emptyBoard

board6_13 :: Board
board6_13 = putBlack (MakeCoordinate 1 3) $ putColAt Black 2 [1..2]  board6

play34 :: Maybe Board
play34 = tryMove White (MakeCoordinate 3 4) startBoard

expected34 :: Board
expected34 = putColAt White 4 [3..4] startBoard


unitTests :: TestTree
unitTests = testGroup "HUnit tests" [
  testCase "First legal turn" $
  play34 @?= Just expected34,
  testCase "Black 1 3 after board 6" $
  tryMove Black (MakeCoordinate 1 3) board6 @?= Just board6_13,
  testCase "Illegal empty move" $
  tryMove Black (MakeCoordinate 1 1) startBoard @?= Nothing,
  testCase "Illegal occupied move" $
  tryMove Black (MakeCoordinate 4 4) startBoard @?= Nothing
                                    ]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
