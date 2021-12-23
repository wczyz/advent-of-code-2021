module Main where

import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Text
import System.Environment

type Board = [[(Int, Bool)]]

rotate array =
    let elementLength = length $ head array
     in [map (!! i) array | i <- [0 .. elementLength - 1]]

isBoardFinished :: Board -> Bool
isBoardFinished board =
    let boardBool :: [[Bool]]
        boardBool = map (map snd) board
        rows :: [Bool]
        rows = map and boardBool
        cols = map and $ rotate boardBool
     in or $ rows ++ cols

markOnBoard :: Int -> Board -> Board
markOnBoard n = map (map (markElement n))
  where
    markElement n (value, b) = if value == n then (value, True) else (value, b)

boardSolutionLength :: [Int] -> Board -> Maybe (Int, Board)
boardSolutionLength drawnNumbers board = boardSolutionLength' board drawnNumbers 0
  where
    boardSolutionLength' :: Board -> [Int] -> Int -> Maybe (Int, Board)
    boardSolutionLength' _ [] _ = Nothing
    boardSolutionLength' board (n : drawnNumbers) result =
        if isBoardFinished board
            then Just (result, board)
            else boardSolutionLength' (markOnBoard n board) drawnNumbers (result + 1)

boardScore :: Int -> Board -> Int
boardScore drawnNumber board =
    let filteredBoard :: Board
        filteredBoard = map (filter ((== False) . snd)) board
        boardSum = sum $ map (sum . map fst) filteredBoard
     in boardSum * drawnNumber

solution strategy drawnNumbers boards =
    let goodBoards = filter ((/= Nothing) . boardSolutionLength drawnNumbers) boards
        goodBoardLengths :: [(Int, Board)]
        goodBoardLengths = map (fromJust . boardSolutionLength drawnNumbers) goodBoards
        (boardLength, winningBoard) = strategy (compare `on` fst) goodBoardLengths
        drawnNumber = drawnNumbers !! (boardLength - 1)
     in boardScore drawnNumber winningBoard

part1 = solution minimumBy
part2 = solution maximumBy

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
     in ys : chunks n zs

main :: IO ()
main = do
    args <- getArgs
    let part = head args
    content <- readFile "input"
    let input = filter (not . null) $ lines content
        drawnNumbers :: [Int]
        drawnNumbers = map (read . Data.Text.unpack) $ Data.Text.splitOn (Data.Text.pack ",") (Data.Text.pack $ head input) :: [Int]
        boards :: [Board]
        boards = chunks 5 $ map ((\xs -> zip xs (repeat False)) . (\xs -> map read xs :: [Int]) . words) $ tail input

    if part == "a"
        then print $ part1 drawnNumbers boards
        else print $ part2 drawnNumbers boards
