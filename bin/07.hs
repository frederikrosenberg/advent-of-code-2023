{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

import Data.Text (Text, pack)
import Data.List
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (replicateM)
import GHC.SourceGen (where')
import Data.Ord (comparing)


type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

data Card = Ace
    | King
    | Queen
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | Jack
    deriving(Eq, Show, Ord)

pCard :: Parser Card
pCard = choice
    [ Ace   <$ char 'A'
    , King  <$ char 'K'
    , Queen <$ char 'Q'
    , Jack  <$ char 'J'
    , Ten   <$ char 'T'
    , Nine  <$ char '9'
    , Eight <$ char '8'
    , Seven <$ char '7'
    , Six   <$ char '6'
    , Five  <$ char '5'
    , Four  <$ char '4'
    , Three <$ char '3'
    , Two   <$ char '2']


data RawHand = RawHand {
    hand :: [Card],
    bet' :: Int
} deriving(Show)


pRawHand :: Parser RawHand
pRawHand = do
    hand <- replicateM 5 pCard
    char ' '
    bet  <- lexeme L.decimal
    return RawHand {hand=hand, bet'=bet}

newtype Input = Input {
    hands :: [RawHand]
} deriving(Show)

pInput :: Parser Input
pInput = do
    hands <- many pRawHand
    return Input {hands=hands}


data HandType = HandFive
    | HandFour
    | HandFull
    | HandThree
    | HandTwoPair
    | HandOnePair
    | HandHigh
    deriving(Eq, Show, Ord)

main = do
    content <- readFile "../input/07.txt"
    let result = parse pInput "" (pack content)

    case result of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right parsedContent -> print $ part2 parsedContent

part1 :: Input -> Int
part1 input = sum $ zipWith (*) [1..] (map bet $ reverse $ sort $ map toHand (hands input))

part2 :: Input -> Int
part2 input = sum $ zipWith (*) [1..] (map bet $ reverse $ sort $ map (toBestHand . toHand) (hands input))

data Hand = Hand {
    handType :: HandType,
    rawHand :: [Card],
    bet :: Int
} deriving (Show, Eq, Ord)

countJ :: [Card] -> Int
countJ c = length $ filter (== Jack) c

toHandType' :: [Int] -> HandType
toHandType' [5] = HandFive
toHandType' [1,4] = HandFour
toHandType' [2,3] = HandFull
toHandType' [1,1,3] = HandThree
toHandType' [1,2,2] = HandTwoPair
toHandType' [1,1,1,2] = HandOnePair
toHandType' _ = HandHigh

toHandType :: [Card] -> HandType
toHandType cards = do
    let x = sort $ map length $ group $ sort cards
    toHandType' x

toHand :: RawHand -> Hand
toHand h = Hand {handType=toHandType (hand h), rawHand=hand h, bet=bet' h}

toBestHand :: Hand -> Hand
toBestHand h = Hand {handType=toBestHandType (handType h) (countJ (rawHand h)), rawHand=rawHand h, bet=bet h}

toBestHandType :: HandType -> Int -> HandType
toBestHandType h 0 = h
toBestHandType HandFive _ = HandFive
toBestHandType HandFull _ = HandFive
toBestHandType HandFour _ = HandFive
toBestHandType HandThree _ = HandFour
toBestHandType HandTwoPair 2 = HandFour
toBestHandType HandTwoPair 1 = HandFull
toBestHandType HandOnePair _ = HandThree
toBestHandType HandHigh _ = HandOnePair
toBestHandType h i = error ("Error unmatched case: " ++ show h ++ ", " ++ show i)
