module Input where

import Protolude

import Text.Parsec.Text (Parser)
import Text.Parsec (many1, digit, space, char, eof, parse, unexpected)
import Coordinate

data Pass = Pass deriving Show

type Input = Either Pass Coordinate


coordParser :: Parser Input
coordParser = do
  c1 <- many1 digit
  _ <- space
  c2 <- many1 digit
  let mcoordinate = liftA2 MakeCoordinate (readMaybe c1) (readMaybe c2)
  case mcoordinate of
    Nothing -> unexpected "should not happen"
    Just coordinate -> return (Right coordinate)

passParser :: Parser Input
passParser = do
  _ <- char 'x'
  _ <- eof
  return (Left Pass)

inputParser :: Parser Input
inputParser = passParser <|> coordParser

parseInput :: Text -> Maybe Input
parseInput s = res parsed where
  parsed = parse inputParser "" s
  res (Left _) = Nothing
  res (Right i) = Just i
