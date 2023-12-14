{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import GHC.SourceGen (do')
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

data Range = Range
  { start :: Int,
    len :: Int
  }
  deriving (Show, Eq, Ord)

data SourceMap = SourceMap
  { input :: Int,
    output :: Int,
    range :: Int,
    mapValue :: Int
  }
  deriving (Show, Eq, Ord)

pSourceMap :: Parser SourceMap
pSourceMap = do
  o <- lexeme L.decimal
  i <- lexeme L.decimal
  r <- lexeme L.decimal
  let m = o - i
  return SourceMap {input = i, output = o, range = r, mapValue = m}

data Input = Input
  { seeds :: [Int],
    seedToSoil :: [SourceMap],
    soilToFert :: [SourceMap],
    fertToWater :: [SourceMap],
    waterToLight :: [SourceMap],
    lightToTemp :: [SourceMap],
    tempToHum :: [SourceMap],
    humToLocation :: [SourceMap]
  }
  deriving (Show)

pInput :: Parser Input
pInput = do
  symbol "seeds:"
  seeds <- many (lexeme L.decimal)
  symbol "seed-to-soil map:"
  seedToSoil <- many pSourceMap
  symbol "soil-to-fertilizer map:"
  soilToFert <- many pSourceMap
  symbol "fertilizer-to-water map:"
  fertToWater <- many pSourceMap
  symbol "water-to-light map:"
  waterToLight <- many pSourceMap
  symbol "light-to-temperature map:"
  lightToTemp <- many pSourceMap
  symbol "temperature-to-humidity map:"
  tempToHum <- many pSourceMap
  symbol "humidity-to-location map:"
  humToLocation <- many pSourceMap
  return Input {seeds = seeds, seedToSoil = seedToSoil, soilToFert = soilToFert, fertToWater = fertToWater, waterToLight = waterToLight, lightToTemp = lightToTemp, tempToHum = tempToHum, humToLocation = humToLocation}

main = do
  content <- readFile "../input/05.txt"
  let result = parse pInput "" (pack content)
  case result of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right p -> do
      let pipeline = [seedToSoil p, soilToFert p, fertToWater p, waterToLight p, lightToTemp p, tempToHum p, humToLocation p]
      print (part1 p pipeline)
      print (part2 p pipeline)

part1 :: Input -> [[SourceMap]] -> Int
part1 p pipeline = do
  let results = map (\x -> foldl mapToNext x pipeline) (seeds p)
      result = minimum results
  result

part2 :: Input -> [[SourceMap]] -> Int
part2 p pipeline = do
  let ranges = map (: []) $ intToRange (seeds p)
  let r = concatMap (\x -> foldl mapToNext' x pipeline) ranges
  let min = minimum r
  start min

intToRange :: [Int] -> [Range]
intToRange [] = []
intToRange xs = Range {start = head xs, len = xs !! 1} : intToRange (drop 2 xs)

mapToNext' :: [Range] -> [SourceMap] -> [Range]
mapToNext' ranges sourceMap = concatMap (mapToNextSingle sourceMap) ranges

mapToNextSingle :: [SourceMap] -> Range -> [Range]
mapToNextSingle sourceMap r = do
  let min = minimum sourceMap
      max = maximum sourceMap
      below = if input min > start r then Just Range {start = start r, len = input min - start r} else Nothing
      above = if input max + range max < start r + len r then Just r else Nothing
      l = map (mapRangeSourceMap r) sourceMap
      result = catMaybes (above : below : l)
  result

mapRangeSourceMap :: Range -> SourceMap -> Maybe Range
mapRangeSourceMap r m = do
  let s = max (start r) (input m)
  let e = min (start r + len r) (input m + range m)
  if s < e
    then Just Range {start = s + mapValue m, len = e - s}
    else Nothing

mapToNext :: Int -> [SourceMap] -> Int
mapToNext value sourceMap = do
  let m = find (\x -> input x <= value && value < input x + range x) sourceMap
  case m of
    Just found -> value + mapValue found
    Nothing -> value
