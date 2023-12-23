{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Data.Char (ord)
import Data.List
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector.Unboxed as VU


type Parser = Parsec Void Text

data Operation = Remove | Upsert {value :: Int} deriving (Show, Eq)

data Lens = Lens { lLabel :: String, lValue :: Int } deriving (Show, Eq)

type Vect = VU.Vector [Lens]

pUpsert :: Parser Operation
pUpsert = do
  char '='
  v <- L.decimal
  return Upsert {value = v}

pOperation :: Parser Operation
pOperation =
  choice
    [ Remove <$ char '-',
      pUpsert
    ]

data Step = Step
  { label :: String,
    op :: Operation
  }
  deriving (Show, Eq)

pStep :: Parser Step
pStep = do
  l <- many alphaNumChar
  o <- pOperation
  return Step {label = l, op = o}

newtype Input = Input
  { steps :: [Step]
  }
  deriving (Show)

pInput :: Parser Input
pInput = do
  r <- pStep `sepEndBy` char ','
  return Input {steps = r}

main = do
  content <- readFile "../input/15.txt"
  let result = parse pInput "" (pack content)
  case result of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right p -> do
        print $ sum (map (hash . fullStr) (steps p))
        print $ hash "pc" 

fullStr :: Step -> String
fullStr s =
  label s ++ case op s of
    Remove -> "-"
    Upsert v -> "=" ++ show v

hash :: String -> Int
hash = foldl (\c s -> ((c + ord s) * 17) `mod` 256) 0

part2 :: Input -> Int
part2 i = do
    let vect = VU.replicate []
