module Main where

import Data.String
import System.Environment

data Direction = Forward | Down | Up
type Movement = (Direction, Int)
type Position = (Int, Int)

evaluate :: [Movement] -> Position
evaluate = foldl f (0, 0)
  where
    f (x, y) (Forward, amount) = (x + amount, y)
    f (x, y) (Down, amount) = (x, y + amount)
    f (x, y) (Up, amount) = (x, y - amount)

strToMovement :: String -> Movement
strToMovement str =
    let [movementString, amountString] = words str
        movement =
            case movementString of
                "forward" -> Forward
                "down" -> Down
                "up" -> Up
     in (movement, read amountString :: Int)

evaluateAngle :: [Movement] -> Position
evaluateAngle xs = fst $ foldl f ((0, 0), 0) xs
  where
    f ((x, y), a) (Forward, amount) = ((x + amount, y + amount * a), a)
    f ((x, y), a) (Down, amount) = ((x, y), a + amount)
    f ((x, y), a) (Up, amount) = ((x, y), a - amount)

solution :: ([Movement] -> Position) -> [String] -> Int
solution eval xs = let (x, y) = eval $ map strToMovement xs in x * y

part1 = solution evaluate
part2 = solution evaluateAngle

main :: IO ()
main = do
    args <- getArgs
    let part = head args
    content <- readFile "input"
    let directions = lines content
    if part == "a"
        then print $ part1 directions
        else print $ part2 directions
