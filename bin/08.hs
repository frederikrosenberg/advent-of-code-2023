{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack)
import Data.List
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Data.HashMap.Strict (HashMap, fromList, (!), lookup, keys)
import Control.Monad (replicateM, void)
import Data.Maybe (fromJust)


type Parser = Parsec Void Text



spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer


data Dir = DirLeft | DirRight deriving(Show,Eq)

data LeftRight = LeftRight {
    left :: String,
    right :: String
} deriving (Eq, Show)

pDir :: Parser Dir
pDir = choice [ DirLeft <$ char 'L', DirRight <$ char 'R' ]

pLeftRight :: Parser (String, LeftRight)
pLeftRight = do
    key <- lexeme (replicateM 3 L.charLiteral)
    symbol "="
    symbol "("
    l <- lexeme (replicateM 3 L.charLiteral)
    symbol ","
    r <- lexeme (replicateM 3 L.charLiteral)
    symbol ")"
    return (key, LeftRight {left=l, right=r})
data Input = Input {
    directions :: [Dir],
    dirLen :: Int,
    nodes :: HashMap String LeftRight
} deriving (Show)

pInput :: Parser Input
pInput = do
    dirs <- many (lexeme pDir)
    pairs <- many pLeftRight
    return Input {directions=dirs, dirLen=length dirs, nodes=fromList pairs}

main = do
    content <- readFile "../input/08.txt"
    let result = parse pInput "" (pack content)

    case result of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right parsedContent -> do
            print $ part1 parsedContent "AAA" 0
            let repeats = map (\x -> part2' parsedContent x 0) (filter isStart (keys $ nodes parsedContent))
            let maxRepeat = maximum repeats
            print $ findNext repeats (maxRepeat * maxRepeat) maxRepeat

part1 :: Input -> String -> Int -> (String, Int)
part1 _ "ZZZ" c = ("ZZZ", c)
part1 i s c = do
    let dir = directions i !! (c `mod` dirLen i)
    let lr = nodes i ! s
    case dir of
        DirLeft -> part1 i (left lr) (c + 1)
        DirRight -> part1 i (right lr) (c + 1)

part2' :: Input -> String -> Int -> Int
part2' i s c
    | isEnd s = c
    | otherwise = do
    let dir = directions i !! (c `mod` dirLen i)
    let lr = nodes i ! s
    case dir of
        DirLeft -> part2' i (left lr) (c + 1)
        DirRight -> part2' i (right lr) (c + 1)

-- Very slow could speed up with prime factors
findNext :: [Int] -> Int -> Int -> Int
findNext l v c
    | all (\x -> v `mod` x == 0) l = v
    | otherwise = findNext l (v + c) c

isEnd :: String -> Bool
isEnd [_, _, 'Z'] = True
isEnd _ = False

isStart :: String -> Bool
isStart [_, _, 'A'] = True
isStart _ = False

step :: Dir -> LeftRight -> String
step DirLeft lr = left lr
step DirRight lr = right lr

