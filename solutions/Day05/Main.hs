module Main where

import Data.List
import qualified Data.Text
import System.Environment

type Point = (Int, Int)
type Line = (Point, Point)

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = (x1 == x2) || (y1 == y2)

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

intervalsCommonSize :: (Int, Int) -> (Int, Int) -> Int
intervalsCommonSize (a, b) (c, d)
    | c > b = 0
    | d < a = 0
    | c >= a && d <= b = d - c + 1
    | a >= c && b <= d = b - a + 1
    | c >= a && c <= b && d >= b = b - c + 1
    | a >= c && a <= d && b >= d = d - a + 1

intersectionPointsCount :: [Line] -> Int
intersectionPointsCount ls =
    let zippedLines = zip ls [0 ..]
        allLinePairs = [(fst x, fst y) | x <- zippedLines, y <- zippedLines, snd x /= snd y]
     in sum $ map countIntersecting allLinePairs
  where
    countIntersecting :: (Line, Line) -> Int
    countIntersecting (l1, l2)
        | isVertical l1 && isVertical l2 =
            let x1 = fst $ fst l1
                x2 = fst $ fst l2
                y11 = snd $ fst l1
                y12 = snd $ snd l1
                y21 = snd $ fst l2
                y22 = snd $ snd l2
             in if x1 == x2
                    then intervalsCommonSize (min y11 y12, max y11 y12) (min y21 y22, max y21 y22)
                    else 0
        | isHorizontal l1 && isHorizontal l2 =
            let y1 = snd $ fst l1
                y2 = snd $ fst l2
                x11 = fst $ fst l1
                x12 = fst $ snd l1
                x21 = fst $ fst l2
                x22 = fst $ snd l2
             in if y1 == y2
                    then intervalsCommonSize (min x11 x12, max x11 x12) (min x21 x22, max x21 x22)
                    else 0
        | isVertical l1 && isHorizontal l2 =
            let y11 = snd $ fst l1
                y21 = snd $ fst l2
                y12 = snd $ snd l1
                y22 = snd $ snd l2
                x11 = fst $ fst l1
                x12 = fst $ snd l1
                x21 = fst $ fst l2
                x22 = fst $ snd l2
             in 0
        | isHorizontal l1 && isVertical l2 = countIntersecting (l2, l1)
        | otherwise = 0

part1 input =
    let straightLines = filter isStraight input
     in intersectionPointsCount straightLines
part2 = part1

strToInt :: String -> Int
strToInt str = read str :: Int

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

main :: IO ()
main = do
    args <- getArgs
    let part = head args
    content <- readFile "input"
    let input =
            map
                ( tuplify2
                    . map
                        ( tuplify2 . map (strToInt . Data.Text.unpack) . Data.Text.splitOn (Data.Text.pack ",")
                        )
                    . Data.Text.splitOn (Data.Text.pack " -> ")
                    . Data.Text.pack
                )
                $ lines content ::
                [Line]

    if part == "a"
        then print $ part1 input
        else print $ part2 input
