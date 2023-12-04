{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.Char
import Data.List

main = do 
    content <- readFile "../input/01.txt"
    let l = lines content
        digits_1 = map oneNumbers l
        total_1 = getSum digits_1

        digits_2 = map numbers l
        total_2 = getSum digits_2

    print total_1
    print total_2

oneNumbers :: String -> [Int]
oneNumbers [] = []
oneNumbers (l:ls) = if ord l >= 48 && ord l <= 57
    then (ord l - 48): oneNumbers ls
    else oneNumbers ls

numbers :: String -> [Int]
numbers [] = []
numbers (stripPrefix "one" -> Just ls) = 1:numbers ("ne" ++ ls)
numbers (stripPrefix "two" -> Just ls) = 2:numbers ("wo" ++ ls)
numbers (stripPrefix "three" -> Just ls) = 3:numbers ("hree" ++ ls)
numbers (stripPrefix "four" -> Just ls) = 4:numbers ("our" ++ ls)
numbers (stripPrefix "five" -> Just ls) = 5:numbers ("ive" ++ ls)
numbers (stripPrefix "six" -> Just ls) = 6:numbers ("ix" ++ ls)
numbers (stripPrefix "seven" -> Just ls) = 7:numbers ("even" ++ ls)
numbers (stripPrefix "eight" -> Just ls) = 8:numbers ("ight" ++ ls)
numbers (stripPrefix "nine" -> Just ls) = 9:numbers ("ine" ++ ls)
numbers (l:ls) = if ord l >= 48 && ord l <= 57
    then (ord l - 48): numbers ls
    else numbers ls

getNum :: [Int] -> Int
getNum list = 10 * head list + last list

getSum :: [[Int]] -> Int
getSum list = sum $ map getNum list

