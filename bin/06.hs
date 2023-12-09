{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack)
import Data.List
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import System.CPUTime (getCPUTime)
import Text.Printf (printf)


type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

data Input = Input {
    times :: [Int],
    distance :: [Int]
} deriving (Show)

pInput :: Parser Input
pInput = do
    symbol "Time:"
    t <- many (lexeme L.decimal)
    symbol "Distance:"
    d <- many (lexeme L.decimal)
    return Input {times=t, distance=d}

main = do
    content <- readFile "../input/06.txt"
    let result = parse pInput "" (pack content)

    case result of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right parsedContent -> do
            let p1 = part1 parsedContent
            print p1

            let p2 = part2 parsedContent
            print p2

part1 :: Input -> Int
part1 input = do
    let combined = zip (times input) (distance input)
    product $ map numberOfWins combined

part2 :: Input -> Int
part2 input = do
    let t = toNumber (times input)
    let d = toNumber (distance input)
    numberOfWins (t, d)

numberOfWins :: (Int, Int) -> Int
numberOfWins (time, distance) = do
    let min = distance `div` time
        first = takeWhile (\x -> (time - x) * x <= distance) [min-1..(min*2)+1]
        l = takeWhile (\x -> (time - x) * x <= distance) [time, (time-1)..(time-(min*2))]
        firstN = last first + 1
        lastN = last l
    lastN - firstN

toNumber :: [Int] -> Int
toNumber input = read $ concatMap show input
