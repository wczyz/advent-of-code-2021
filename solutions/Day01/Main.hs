module Main where

import System.Environment

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

countIncreasing :: [Int] -> Int
countIncreasing [] = 0
countIncreasing [x] = 0
countIncreasing (x : y : xs) = if' (y > x) 1 0 + countIncreasing (y : xs)

threeSumList :: [Int] -> [Int]
threeSumList [] = []
threeSumList [x] = []
threeSumList [x, y] = []
threeSumList (x : y : z : xs) = (x + y + z) : threeSumList (y : z : xs)

part1 = countIncreasing
part2 xs = countIncreasing $ threeSumList xs

main :: IO ()
main = do
    args <- getArgs
    let part = head args
    content <- readFile "input"
    let numbers = map read (lines content) :: [Int]
    if part == "a"
        then print $ part1 numbers
        else print $ part2 numbers
