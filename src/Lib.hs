module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.Maybe (isJust)

data Player =  Black | White deriving Eq

shortCellState :: Player -> String
shortCellState Black = "●"
shortCellState White = "○"

data Coordinate = MakeCoordinate Int Int deriving (Eq, Ord, Show)
data Vector = MakeVector Int Int deriving (Show)

plus :: Coordinate -> Vector -> Coordinate
plus (MakeCoordinate i j) (MakeVector vi vj) = MakeCoordinate (i+vi) (j+vj)

plusV :: Vector -> Vector -> Vector
plusV (MakeVector i j) (MakeVector vi vj) = MakeVector (i+vi) (j+vj)


instance Show Player where
  show = shortCellState

printMCell :: Maybe Player -> String
printMCell Nothing = "."
printMCell (Just x) = show x


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
  -> [Maybe Player]
getRow size r board = do
  i <- [1..size]
  return (Map.lookup (MakeCoordinate r i) board)

printRow :: [Maybe Player] -> String
printRow l = intercalate " " cellStrings
  where cellStrings = map printMCell l

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


getDirection :: Direction -> Vector
getDirection N = MakeVector (-1)  0
getDirection S = MakeVector 1  0
getDirection E = MakeVector 0  1
getDirection W = MakeVector 0  (-1)
getDirection NW = plusV (getDirection N) (getDirection W)
getDirection NE = plusV (getDirection N) (getDirection E)
getDirection SE = plusV (getDirection S) (getDirection E)
getDirection SW = plusV (getDirection S) (getDirection W)


isEnemy' :: Player -> Maybe Player -> Bool
isEnemy' White (Just Black) = True
isEnemy' Black (Just White) = True
isEnemy' _ _ = False

isMe :: Player -> Maybe Player -> Bool
isMe p (Just p') = p == p'
isMe _ _ = False

isEnemy :: Board -> Player -> Coordinate -> Bool
isEnemy board player coordinate = isEnemy' player (Map.lookup coordinate board)


gatherEnemyCells :: Direction -> Board -> Player -> Coordinate ->  [Coordinate]
gatherEnemyCells direction board player coordinate = getResult (reverse bothEnemy) where
  d = getDirection direction
  coordList = tail (iterate (`plus` d) coordinate)
  playerList = map (`Map.lookup` board) coordList
  both = zip coordList (tail playerList)
  bothEnemy = takeWhile ((isEnemy board player) . fst) both
  getResult [] = []
  getResult l@(x:_) = if isMe player (snd x)
            then map fst l
            else []

gatherAllEnemyCells :: Board -> Player -> Coordinate -> [Coordinate]
gatherAllEnemyCells board player coord = concat cells where
  cells = do
    dir <- enumFrom N
    return (gatherEnemyCells dir board player coord)

switch :: Player -> Player
switch Black = White
switch White = Black



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
