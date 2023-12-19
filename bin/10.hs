import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Debug.Trace (trace)

data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)

add :: Point -> Point -> Point
add a b = Point {x = x a + x b, y = y a + y b}

step = Point {x = 1, y = 0}

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
      map = M.fromList $ zip coordinates (concat inputdata)
      start = head $ M.keys $ M.filter (== 'S') map
      p = path start start map
      startChar = getStartChar p map
      newMap = M.insert start startChar map
      pathMap = M.fromList (L.map (\x -> (x, newMap M.! x)) p)
      edges = [Point {x = 0, y = y} | y <- [0 .. length inputdata - 1]]
      inside = L.map (\x -> castRay x (length (head inputdata)) pathMap 0 ' ') edges
  print $ length p `div` 2
  print $ sum inside


getStartChar :: [Point] -> M.Map Point Char -> Char
getStartChar path map = do
    let start = head path
    let c = head $ M.keys $ M.filter (\x -> matches path start (head x) map && matches path start (last x) map) lookupTable
    c

matches :: [Point] -> Point -> Int -> M.Map Point Char -> Bool
matches path p i map = do
    let move = movement L.!! i
    let lookup = add p (fst move)
    map M.! lookup `elem` snd move && L.elem lookup path


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

nextPoint :: Point -> Point -> M.Map Point Char -> [(Point, String)] -> Point
nextPoint cur prev map pos = do
  let next = fromJust (L.find (nextFilter cur prev map) pos)
  add cur (fst next)

nextFilter :: Point -> Point -> M.Map Point Char -> (Point, String) -> Bool
nextFilter cur prev map move = do
  let next = add cur (fst move)
  next /= prev && M.member next map && (map M.! next) `elem` snd move

castRay :: Point -> Int -> M.Map Point Char -> Int -> Char -> Int
castRay cur len pathMap count last
  | x cur == len = 0
  | otherwise = do
    let t = M.lookup cur pathMap
    let (cc, nCount, result) = case t of
          Just c -> do
            if c `elem` inside
              then (c, count + 1, 0)
              else
                if L.elem c outside && not ((last == 'F' && c == 'J') || (last == 'L' && c == '7'))
                  then (last, count + 1, 0)
                  else (last, count, 0)
          Nothing -> (last, count, count `mod` 2)
    result + castRay (add cur step) len pathMap nCount cc

inside = ['|', 'L', 'F']

outside = ['|', 'J', '7']
