import Test.Tasty
import Test.Tasty.HUnit

import Lib 
import Board
import Player
import Coordinate

play34 :: Maybe Board
play34 = tryMove White (MakeCoordinate 3 4) startBoard

expected34 :: Board
expected34 =
  putWhite (MakeCoordinate 3 4) $ putWhite (MakeCoordinate 4 4) startBoard
  where
    putWhite = playAt White


unitTests :: TestTree
unitTests = testGroup "HUnit tests" [
  testCase "First legal turn" $
  play34 @?= Just expected34
                                    ]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
