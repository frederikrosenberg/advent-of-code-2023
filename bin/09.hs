{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

newtype Input = Input
  { history :: [[Int]]
  }
  deriving (Show)

pNumber :: Parser Int
pNumber = do
  n <- L.signed spaceConsumer L.decimal
  optional (char ' ')
  return n

pInput :: Parser Input
pInput = do
  history <- many pNumber `sepEndBy` char '\n'
  return Input {history = filter (/= []) history}

main = do
  content <- readFile "../input/09.txt"
  let result = parse pInput "" (pack content)
  case result of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right p -> do
        print $ sum $ map (sum . nextSeq) (history p)
        print $ sum $ map (foldr (-) 0 . nextFirstSeq) (history p)

nextFirstSeq :: [Int] -> [Int]
nextFirstSeq [] = []
nextFirstSeq i = head i : nextFirstSeq' i


nextFirstSeq' :: [Int] -> [Int]
nextFirstSeq' i
    | all (== 0) i = []
    | otherwise = do
        let n = nextSeq'' i
        head n : nextFirstSeq' n

nextSeq :: [Int] -> [Int]
nextSeq [] = []
nextSeq i = last i : nextSeq' i


nextSeq' :: [Int] -> [Int]
nextSeq' i
    | all (== 0) i = []
    | otherwise = do
        let n = nextSeq'' i
        last n : nextSeq' n


nextSeq'' :: [Int] -> [Int]
nextSeq'' [] = error "Should not hit"
nextSeq'' (x : y : xs) = (y - x) : nextSeq'' (y : xs)
nextSeq'' _ = []
