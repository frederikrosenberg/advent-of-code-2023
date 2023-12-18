import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)

data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)

add :: Point -> Point -> Point
add a b = Point {x = x a + x b, y = y a + y b}

movement =
  [ (Point {x = 0, y = -1}, ['|', '7', 'F', 'S']),
    (Point {x = 1, y = 0}, ['-', 'J', '7', 'S']),
    (Point {x = 0, y = 1}, ['|', 'L', 'J', 'S']),
    (Point {x = -1, y = 0}, ['-', 'F', 'L', 'S'])
  ]

lookupTable =
  M.fromList
    [ ('|', [0, 2]),
      ('-', [1, 3]),
      ('7', [2, 3]),
      ('F', [1, 2]),
      ('J', [0, 3]),
      ('L', [0, 1])
    ]

main = do
  inputdata <- lines <$> readFile "../input/10.txt"
  let coordinates = [Point {x = x, y = y} | y <- [0 .. length inputdata - 1], x <- [0 .. length (head inputdata) - 1]]
  -- And then just zip it up:
  let map = M.fromList $ zip coordinates (concat inputdata)
  let start = head $ M.keys $ M.filter (== 'S') map
  let p = path start start map
  print $ length p `div` 2

path :: Point -> Point -> M.Map Point Char -> [Point]
path cur prev map
  | (map M.! cur) == 'S' && cur /= prev = [prev]
  | (map M.! cur) == 'S' = do
    let next = nextPoint cur prev map movement
    path next cur map
  | otherwise = do
    let c = map M.! cur
        t = lookupTable M.! c
        pos = [movement !! head t, movement !! last t]
        next = nextPoint cur prev map pos
    prev : path next cur map

nextPoint :: Point -> Point -> M.Map Point Char -> [(Point, [Char])] -> Point
nextPoint cur prev map pos = do
  let next = fromJust (L.find (nextFilter cur prev map) pos)
  add cur (fst next)

nextFilter :: Point -> Point -> M.Map Point Char -> (Point, [Char]) -> Bool
nextFilter cur prev map move = do
    let next = add cur (fst move)
    next /= prev && M.member next map && (map M.! next) `elem` snd move
