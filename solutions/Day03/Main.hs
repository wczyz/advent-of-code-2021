module Main where

import Data.Bifunctor
import System.Environment

type BinaryNumber = [Bool]

mostCommon :: BinaryNumber -> Bool
mostCommon xs =
    let count :: Eq a => a -> [a] -> Int
        count x = (length . filter (== x))
     in count True xs * 2 >= length xs

rotateBinaryNumbersList binaryNumbers =
    let elementLength = length $ head binaryNumbers
     in [map (!! i) binaryNumbers | i <- [0 .. elementLength - 1]]

getGammaEpsilon :: [BinaryNumber] -> (Int, Int)
getGammaEpsilon binaryNumbers = foldl f (0, 0) $ map mostCommon $ rotateBinaryNumbersList binaryNumbers
  where
    f (x, y) b =
        if b
            then (x * 2 + 1, y * 2)
            else (x * 2, y * 2 + 1)

binaryToInt :: BinaryNumber -> Int
binaryToInt = foldl (\x b -> if b then x * 2 + 1 else x * 2) 0

stepOxygen :: [(BinaryNumber, Int)] -> Int
stepOxygen pairs =
    if length pairs == 1
        then snd $ head pairs
        else
            let binaryNumbers = map fst pairs
                firstElements = map head binaryNumbers
                mostCommonElement = mostCommon firstElements
                filteredPairs = filter ((== mostCommonElement) . head . fst) pairs
             in stepOxygen $ map (Data.Bifunctor.first tail) filteredPairs

stepCo2 :: [(BinaryNumber, Int)] -> Int
stepCo2 pairs =
    if length pairs == 1
        then snd $ head pairs
        else
            let binaryNumbers = map fst pairs
                firstElements = map head binaryNumbers
                leastCommonElement = not $ mostCommon firstElements
                filteredPairs = filter ((== leastCommonElement) . head . fst) pairs
             in stepCo2 $ map (Data.Bifunctor.first tail) filteredPairs

getIndex :: [BinaryNumber] -> ([(BinaryNumber, Int)] -> Int) -> Int
getIndex binaryNumbers stepStrategy = stepStrategy $ zip binaryNumbers [0 ..]

getOxygenCo2 :: [BinaryNumber] -> (Int, Int)
getOxygenCo2 binaryNumbers =
    let oxygenBinary = binaryNumbers !! getIndex binaryNumbers stepOxygen
        co2Binary = binaryNumbers !! getIndex binaryNumbers stepCo2
     in (binaryToInt oxygenBinary, binaryToInt co2Binary)

solution eval binaryNumbers = let (x, y) = eval binaryNumbers in x * y

part1 = solution getGammaEpsilon
part2 = solution getOxygenCo2

strToBinary :: String -> BinaryNumber
strToBinary = map (== '1')

main :: IO ()
main = do
    args <- getArgs
    let part = head args
    content <- readFile "input"
    let binaryNumbers :: [BinaryNumber]
        binaryNumbers = map strToBinary $ lines content
    if part == "a"
        then print $ part1 binaryNumbers
        else print $ part2 binaryNumbers
