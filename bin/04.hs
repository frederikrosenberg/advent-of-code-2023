{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack)
import Data.List
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text



spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

data Card = Card {
    number :: Int,
    winning :: [Int],
    have :: [Int]
} deriving (Eq, Show)

pCard :: Parser Card
pCard = do
    _ <- symbol "Card"
    number <- lexeme L.decimal
    _ <- symbol ":"
    winning <- many (lexeme L.decimal)
    _ <- symbol "|"
    have <- many (lexeme L.decimal)
    return Card {number=number, winning=winning, have=have}

newtype Input = Input {
    cards :: [Card]
} deriving (Show)

pInput :: Parser Input
pInput = do
    i <- many (try (pCard <* spaceConsumer))
    eof
    return (Input i)

main = do
    content <- readFile "../input/04.txt"
    let result = parse pInput "" (pack content)

    case result of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right parsedContent -> do
            let p1 = sum $ map part1 (cards parsedContent)
            print p1

            let p2 = foldl part2 defaultCopies (cards parsedContent)
            print (total p2)

winnings :: Card -> Int
winnings c = length (filter (\x -> x `elem` winning c) (have c))

part1 :: Card -> Int
part1 c = do
    let x = winnings c
    if x == 0 then 0 else 2^(x-1)

data CopiesAgrregate = Copies {
    total :: Int,
    extras :: [Int]
} deriving (Show, Eq)

defaultCopies :: CopiesAgrregate
defaultCopies = Copies {total=0, extras=[]}

part2 :: CopiesAgrregate -> Card -> CopiesAgrregate
part2 agrr card = do
    let wins = winnings card
    let (copies, rest) = headAndRest (extras agrr)
    let e = replicate wins (copies + 1)
    let extras = addCombine rest e
    Copies {total=total agrr + 1 + copies, extras=extras}

addCombine :: [Int] -> [Int] -> [Int]
addCombine (x:xs) (y:ys) = x + y : addCombine xs ys
addCombine []     ys     = ys
addCombine xs     []     = xs

headAndRest :: [Int] -> (Int, [Int])
headAndRest [] = (0, [])
headAndRest (x:rest) = (x, rest)


