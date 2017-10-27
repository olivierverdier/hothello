module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.Maybe (isJust)

data Player =  Black | White deriving Eq

switch :: Player -> Player
switch Black = White
switch White = Black


type Cell = Maybe Player

data Coordinate = MakeCoordinate Int Int deriving (Eq, Ord, Show)
data Vector = MakeVector Int Int deriving (Show)

plus :: Coordinate -> Vector -> Coordinate
plus (MakeCoordinate i j) (MakeVector vi vj) = MakeCoordinate (i+vi) (j+vj)

plusV :: Vector -> Vector -> Vector
plusV (MakeVector i j) (MakeVector vi vj) = MakeVector (i+vi) (j+vj)


instance Show Player where
  show Black = "●"
  show White = "○"

printCell :: Cell -> String
printCell Nothing = "."
printCell (Just x) = show x


type Board = Map.Map Coordinate Player



emptyBoard :: Board
emptyBoard = Map.empty

startBoard :: Board
startBoard = Map.fromList [
  (MakeCoordinate 4 4, Black),
  (MakeCoordinate 4 5, White),
  (MakeCoordinate 5 4, White),
  (MakeCoordinate 5 5, Black)]

getRow :: Int -- row size
  -> Int -- row
  -> Board
  -> [Cell]
getRow size r board = do
  i <- [1..size]
  return (Map.lookup (MakeCoordinate r i) board)

printRow :: [Cell] -> String
printRow l = intercalate " " cellStrings
  where cellStrings = fmap printCell l

printBoard :: Coordinate -> Board -> String
printBoard (MakeCoordinate m n) board = intercalate "\n" rows
  where
    rows = do
      rowNb <- [1..m]
      let row = getRow n rowNb board
      return (printRow row)

printStdBoard :: Board -> String
printStdBoard = printBoard (MakeCoordinate 8 8)

-- play :: Board -> Coordinate -> Board

-- type Direction = Coordinate
data Direction = N | S | E | W | NW | NE | SE | SW deriving Enum


getVector :: Direction -> Vector
getVector N = MakeVector (-1)  0
getVector S = MakeVector 1  0
getVector E = MakeVector 0  1
getVector W = MakeVector 0  (-1)
getVector NW = plusV (getVector N) (getVector W)
getVector NE = plusV (getVector N) (getVector E)
getVector SE = plusV (getVector S) (getVector E)
getVector SW = plusV (getVector S) (getVector W)

isSamePlayerAs :: Player -> Cell -> Bool
isSamePlayerAs p (Just p') = p == p'
isSamePlayerAs _ Nothing = False

hasPlayerAt :: Board -> Player -> Coordinate -> Bool
hasPlayerAt board player coordinate = isSamePlayerAs player (Map.lookup coordinate board)


gatherEnemyCells :: Direction -> Board -> Player -> Coordinate ->  [Coordinate]
gatherEnemyCells direction board player coordinate = getResult (reverse bothEnemy) where
  d = getVector direction
  coordList = tail (iterate (`plus` d) coordinate)
  playerList = fmap (`Map.lookup` board) coordList
  both = zip coordList (tail playerList)
  enemy = switch player
  bothEnemy = takeWhile ((hasPlayerAt board enemy) . fst) both
  getResult [] = []
  getResult l@(x:_) = if isSamePlayerAs player (snd x)
            then fmap fst l
            else []

gatherAllEnemyCells :: Board -> Player -> Coordinate -> [Coordinate]
gatherAllEnemyCells board player coord = concat cells where
  cells = do
    dir <- enumFrom N
    return (gatherEnemyCells dir board player coord)



swap :: Coordinate -> Board -> Board
swap coord board = newBoard mPlayer where
  maybePlayer = Map.lookup coord board
  mPlayer = fmap switch maybePlayer
  newBoard Nothing = board
  newBoard (Just p) = Map.insert coord p board


move :: Board -> Player -> Coordinate -> Maybe Board
move board me coord = result where
  gathered = gatherAllEnemyCells board me coord
  illegalMove = null gathered || isJust (Map.lookup coord board)
  swapped = foldr swap board gathered
  newBoard = Map.insert coord me swapped
  result = if illegalMove then Nothing else Just newBoard






 





someFunc :: IO ()
someFunc = putStrLn "someFunc"
