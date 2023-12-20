import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Debug.Trace (trace)

data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)

add :: Point -> Point -> Point
add a b = Point {x = x a + x b, y = y a + y b}

stepDist :: Point -> Point -> Int
stepDist a b = do
  let dx = abs (x a - x b)
  let dy = abs (y a - y b)
  dx + dy

stepRow = Point {x = 1, y = 0}

stepCol = Point {x = 0, y = 1}

data Grid = Grid
  { content :: M.Map Point Char,
    width :: Int,
    height :: Int
  }
  deriving (Show)

main = do
  inputdata <- lines <$> readFile "../input/11.txt"
  let coordinates = [Point {x = x, y = y} | y <- [0 .. length inputdata - 1], x <- [0 .. length (head inputdata) - 1]]
      content = M.fromList $ zip coordinates (concat inputdata)
      grid = Grid {content = content, width = length (head inputdata), height = length inputdata}
      emptyRows = L.filter (\x -> isRowEmpty Point {x = 0, y = x} grid) [0 .. width grid - 1]
      emptyCols = L.filter (\x -> isColEmpty Point {x = x, y = 0} grid) [0 .. height grid - 1]
      galaxies = M.keys $ M.filter (== '#') content
      galaxies1 = L.map (\p -> movePoint p 1 emptyRows emptyCols) galaxies
      galaxies2 = L.map (\p -> movePoint p 999999 emptyRows emptyCols) galaxies
  print $ dists galaxies1
  print $ dists galaxies2

dists :: [Point] -> Int
dists [] = 0
dists [_] = 0
dists (p : xs) = do
  let r = sum $ L.map (stepDist p) xs
  r + dists xs

movePoint :: Point -> Int -> [Int] -> [Int] -> Point
movePoint p f rows cols = do
  let overRows = L.length (L.filter (<= y p) rows) * f
      overCols = L.length (L.filter (<= x p) cols) * f
  Point {x = x p + overCols, y = y p + overRows}

isRowEmpty :: Point -> Grid -> Bool
isRowEmpty start grid
  | x start == width grid = True
  | content grid M.! start == '#' = False
  | otherwise = isRowEmpty (add start stepRow) grid

isColEmpty :: Point -> Grid -> Bool
isColEmpty start grid
  | y start == height grid = True
  | content grid M.! start == '#' = False
  | otherwise = isColEmpty (add start stepCol) grid
