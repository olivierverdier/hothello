module Input where

import Text.Parsec.String
import Text.Parsec (many1, digit, space, char, eof, (<|>), parse)
import Coordinate

data Pass = Pass deriving Show

type Input = Either Pass Coordinate


coordParser :: Parser Input
coordParser = do
  c1 <- many1 digit
  _ <- space
  c2 <- many1 digit
  let coordinate = MakeCoordinate (read c1) (read c2)
  return (Right coordinate)

passParser :: Parser Input
passParser = do
  _ <- char 'x'
  _ <- eof
  return (Left Pass)

inputParser :: Parser Input
inputParser = passParser <|> coordParser

parseInput :: String -> Maybe Input
parseInput s = res parsed where
  parsed = parse inputParser "" s
  res (Left _) = Nothing
  res (Right i) = Just i
