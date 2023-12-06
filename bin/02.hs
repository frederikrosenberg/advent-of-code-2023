{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack)
import Data.List
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


data Game = Game {
    id :: Int,
    subsets :: [Subset]
} deriving(Eq, Show)

pGame :: Parser Game
pGame = do
    _ <- string "Game "
    id <- lexeme L.decimal
    _ <- string ": "
    s <- pSubset `sepBy` lexeme ";"
    return (Game id s)


data Color = ColorRed | ColorBlue | ColorGreen deriving(Eq, Show)

pColor :: Parser Color
pColor = choice 
    [ ColorRed   <$ string "red"
    , ColorBlue  <$ string "blue"
    , ColorGreen <$ string "green" ]

data Cube = Cube
    { count :: Int
    , color :: Color } deriving(Eq, Show)

pCube :: Parser Cube
pCube = do
    n <- lexeme L.decimal
    Cube n <$> pColor

newtype Subset = Subset
    { cubes :: [Cube] } deriving(Eq, Show)

pSubset :: Parser Subset
pSubset = do
    c <- pCube `sepBy` lexeme ","
    return (Subset c)


newtype Input = Input {
    games :: [Game]
} deriving(Eq, Show)

pInput :: Parser Input
pInput = do
    i <- many (try (pGame <* sc))
    eof
    return (Input i)

main = do 
    content <- readFile "../input/02.txt"
    let result = parse pInput "" (pack content)

    case result of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right parsedContent -> do
            let valid = filter isValid (games parsedContent)
                total = sum $ map getId valid
        
            print total

            let counts = sum $ map gamePower (games parsedContent)
            print counts

getId :: Game -> Int
getId (Game id _) = id 

isValid :: Game -> Bool
isValid (Game id subsets) = all isSubsetValid subsets

isSubsetValid :: Subset -> Bool
isSubsetValid (Subset cubes) = all isCubeValid cubes

isCubeValid :: Cube -> Bool
isCubeValid (Cube count ColorRed) = count <= 12
isCubeValid (Cube count ColorGreen) = count <= 13
isCubeValid (Cube count ColorBlue) = count <= 14

gamePower :: Game -> Int
gamePower game = do
    let values = map getCount (subsets game)
        result = foldr maxRgb defaultRgb values
    red result * green result * blue result

data Rgb = Rgb { red :: Int, green :: Int, blue :: Int }

maxRgb :: Rgb -> Rgb -> Rgb
maxRgb a b = Rgb {red=max (red a) (red b), green=max (green a) (green b), blue=max (blue a) (blue b)}

defaultRgb :: Rgb
defaultRgb = Rgb {red = 0, green = 0, blue = 0 }

getCount :: Subset -> Rgb
getCount s = do
    let r = findColor (cubes s) ColorRed
        g = findColor (cubes s) ColorGreen
        b = findColor (cubes s) ColorBlue
        result = Rgb {red=r, green=g, blue=b}
    result


findColor :: [Cube] -> Color -> Int
findColor cubes color = case find (\(Cube _ c) -> c == color) cubes of 
    Just (Cube c _) -> c
    Nothing -> 0
