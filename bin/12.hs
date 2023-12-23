{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Codec.Picture.Metadata (Value(Int))

type Parser = Parsec Void Text

data Condition = Operational | Damaged | Unknown deriving (Show, Eq)

pCondition :: Parser Condition
pCondition =
  choice
    [ Operational <$ char '.',
      Damaged <$ char '#',
      Unknown <$ char '?'
    ]

data Record = Record
  { springs :: [Condition],
    groups :: [Int]
  }
  deriving (Show)

pRecord :: Parser Record
pRecord = do
  s <- many pCondition
  char ' '
  g <- L.decimal `sepBy` char ','
  return Record {springs = s, groups = g}

newtype Input = Input
  { records :: [Record]
  }
  deriving (Show)

pInput :: Parser Input
pInput = do
  r <- pRecord `sepEndBy` char '\n'
  return Input {records = r}

main = do
  content <- readFile "../input/12_1.txt"
  let result = parse pInput "" (pack content)
  case result of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right p -> do
        let arr = map (`arrangements` []) (records p)
            o = map length arr
        print $ sum o
        let e = map filterEnding arr
            f = map (length . (`arrangements` []) . toTest) (records p)
            l = map (length . (`arrangements` []) . toTest1) (records p)
            z = zip4 o e f l
            m = map calc z
        print z
        print m
        print $ sum m
        print $ map (length . (`arrangements` []) . toPart2Recordx 2) (records p) 
        print $ map (length . (`arrangements` []) . toPart2Recordx 3) (records p) 
        print $ map (length . (`arrangements` []) . toPart2Recordx 4) (records p) 
        print $ map (length . (`arrangements` []) . toPart2Recordx 5) (records p) 

calc :: (Int, Int, Int, Int) -> Int
calc (o, e, f, l) = if e == 0 && o == 1 then 1 else o * (max f l)^4

filterEnding :: [[Condition]] -> Int
filterEnding c = length (filter (\x -> last x /= Damaged) c)

toTest :: Record -> Record
toTest r = Record {springs=Unknown : springs r, groups=groups r}

toTest1 :: Record -> Record
toTest1 r = Record {springs=springs r ++ [Unknown], groups=groups r}

toPart2Record :: Record -> Record
toPart2Record r = do
    let s = concat $ replicate 4 (springs r ++ [Unknown])
    let g = concat $ replicate 5 (groups r)
    Record {springs=s ++ springs r, groups=g}

toPart2Recordx :: Int -> Record -> Record
toPart2Recordx c r = do
    let s = concat $ replicate (c-1) (springs r ++ [Unknown])
    let g = concat $ replicate c (groups r)
    Record {springs=s ++ springs r, groups=g}

toPart2Record1 :: Record -> Record
toPart2Record1 r = Record {springs=springs r ++ [Unknown] ++ springs r, groups=groups r ++ groups r}

toPart2Record2 :: Record -> Record
toPart2Record2 r = Record {springs=springs r ++ [Unknown] ++ springs r ++ [Unknown] ++ springs r, groups=groups r ++ groups r ++ groups r}

arrangements :: Record -> [Condition] -> [[Condition]]
arrangements r c
  | null (springs r) && not (null (groups r)) = []
  | null (groups r) = [c | Damaged `notElem` springs r]
  | head (springs r) == Operational = arrangements (handleOperational r) (c ++ [Operational])
  | head (springs r) == Damaged = do
    let (result, d, cc) = handleDamaged r
    if result
      then arrangements d (c ++ cc)
      else []
  | head (springs r) == Unknown = handleQuestion r c
  | otherwise = error ("Invalid record: " ++ show r)

handleDamaged :: Record -> (Bool, Record, [Condition])
handleDamaged r
  | head (groups r) > length (take (head (groups r) + 1) (springs r)) = (False, r, [])
  | otherwise = do
    let size = head (groups r)
        block = take (size + 1) (springs r)
        fixedBlock = if length block /= (size + 1) then block ++ [Operational] else block
    if blockOk fixedBlock
      then (True, Record {springs = drop (size + 1) (springs r), groups = tail (groups r)}, replicate size Damaged ++ [Operational | length block == length fixedBlock])
      else (False, r, [])

blockOk :: [Condition] -> Bool
blockOk [x] = x /= Damaged
blockOk (x : xs) = x /= Operational && blockOk xs
blockOk [] = error "Failed"

handleQuestion :: Record -> [Condition] -> [[Condition]]
handleQuestion r c = do
  let (result, d, cc) = handleDamaged r
  if result
    then arrangements (handleOperational r) (c ++ [Operational]) ++ arrangements d (c ++ cc)
    else arrangements (handleOperational r) (c ++ [Operational])

handleOperational :: Record -> Record
handleOperational r = Record {springs = tail (springs r), groups = groups r}
