import Protolude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Util (tryMove)

import Board
import Player
import Coordinate




board6 :: Board
board6 = putColAt Black 1 [1..3] $ putColAt White 2 [1..3] emptyBoard

board6_13 :: Board
board6_13 = putBlack (MkCoordinate 1 3) $ putColAt Black 2 [1..2]  board6

expected34 :: Board
expected34 = putColAt White 4 [3..4] startBoard


unitTests :: TestTree
unitTests = testGroup "HUnit tests" [
  testCase "First legal turn" $
  tryMove White (MkCoordinate 3 4) startBoard @?= Just expected34,
  testCase "Black 1 3 after board 6" $
  tryMove Black (MkCoordinate 1 3) board6 @?= Just board6_13,
  testCase "Illegal empty move" $
  tryMove Black (MkCoordinate 1 1) startBoard @?= Nothing,
  testCase "Illegal occupied move" $
  tryMove Black (MkCoordinate 4 4) startBoard @?= Nothing
                                    ]

checkGatheredAreEnemies :: Board -> Player -> Coordinate -> Bool
checkGatheredAreEnemies board player coord = all checks cells where
  coords = gatherAllEnemyCells board player coord
  cells = fmap (getCell board) coords
  checks = isSamePlayerAs (switch player)

qcProps :: TestTree
qcProps = testGroup "QuickCheck tests" [
  QC.testProperty "only enemy gathered" checkGatheredAreEnemies
                                       ]

tests :: TestTree
tests = testGroup "Tests" [unitTests, qcProps]

main :: IO ()
main = defaultMain tests
