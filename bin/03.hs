{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Map (fromList, lookup, member)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State, tokens)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Position = Position {line :: Int, col :: Int} deriving (Show, Eq, Ord)

data PosToken
  = PosSymbol
      { pos :: Position,
        symbol :: Char
      }
  | PosNumber
      { start :: Position,
        number :: Int
      }
  | Dot
  deriving (Show, Eq)

pSymbol :: Parser Char
pSymbol =
  char '-'
    <|> char '+'
    <|> char '/'
    <|> char '&'
    <|> char '#'
    <|> char '='
    <|> char '@'
    <|> char '%'
    <|> char '$'
    <|> char '*'

pToken :: Parser PosToken
pToken = do
  pos <- getSourcePos
  choice
    [ Dot <$ char '.',
      toSymbol (toPosition pos) <$> pSymbol,
      toNumber (toPosition pos) <$> L.decimal
    ]

toSymbol :: Position -> Char -> PosToken
toSymbol s c = PosSymbol {pos = s, symbol = c}

toNumber :: Position -> Int -> PosToken
toNumber s c = PosNumber {start = s, number = c}

toPosition :: SourcePos -> Position
toPosition s = Position {line = unPos (sourceLine s), col = unPos (sourceColumn s)}

newtype Input = Input {tokens :: [PosToken]} deriving (Show)

pInput :: Parser Input
pInput = do
  t <- many pToken `sepEndBy` char '\n'
  return Input {tokens = concat t}

main = do
  content <- readFile "../input/03.txt"
  let result = parse pInput "" (pack content)

  case result of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right parsedContent -> do
      let x = filter (/= Dot) (tokens parsedContent)
          rawSymbols = map toPair $ filter isSymbol x
          symbols = fromList rawSymbols
          gears = filter (\x -> snd x == '*') rawSymbols
          numbers = map toNumberPair $ filter isNumber x
          numbersMap = fromList $ concatMap toNumberPairAll numbers
          matchingSum = sum . map snd $ filter (any (`member` symbols) . aroundNumber) numbers
          gearsSum = sum $ mapMaybe (`gear` numbersMap) gears
      print matchingSum
      print gearsSum

isSymbol :: PosToken -> Bool
isSymbol t = case t of
  PosSymbol _ _ -> True
  _ -> False

toPair :: PosToken -> (Position, Char)
toPair t = case t of
  PosSymbol p c -> (p, c)
  _ -> error "Error"

isNumber :: PosToken -> Bool
isNumber t = case t of
  PosNumber _ _ -> True
  _ -> False

toNumberPair :: PosToken -> (Position, Int)
toNumberPair t = case t of
  PosNumber p c -> (p, c)
  _ -> error "Error"

toNumberPairAll :: (Position, Int) -> [(Position, (Position, Int))]
toNumberPairAll (p, n)
  | n < 10 = [(p, (p, n))]
  | n < 100 = [(p, (p, n)), (Position {line = line p, col = col p + 1}, (p, n))]
  | n < 1000 = [(p, (p, n)), (Position {line = line p, col = col p + 1}, (p, n)), (Position {line = line p, col = col p + 2}, (p, n))]
  | otherwise = []

aroundNumber :: (Position, Int) -> [Position]
aroundNumber (p, n)
  | n < 10 = around p
  | n < 100 = around p ++ around Position {line = line p, col = col p + 1}
  | n < 1000 = around p ++ around Position {line = line p, col = col p + 2}
  | otherwise = []

gear :: (Position, Char) -> Map.Map Position (Position, Int) -> Maybe Int
gear (p, _) m = do
  let numbers = nub $ mapMaybe (`Data.Map.lookup` m) (around p)
  if length numbers == 2 then Just (product $ map snd numbers) else Nothing

around :: Position -> [Position]
around p =
  [ Position {line = line p - 1, col = col p - 1},
    Position {line = line p - 1, col = col p},
    Position {line = line p - 1, col = col p + 1},
    Position {line = line p, col = col p - 1},
    Position {line = line p, col = col p + 1},
    Position {line = line p + 1, col = col p - 1},
    Position {line = line p + 1, col = col p},
    Position {line = line p + 1, col = col p + 1}
  ]
