module P4a where
import Data.List (transpose, find)
import Data.List.Split (splitOn)
import Data.String (String)

data BingoItem = Unmarked Int | Marked deriving Eq

type Board = [[BingoItem]]

diagonal :: [[a]] -> [a]
diagonal xs = zipWith (!!) xs [0..]

isRowBingo :: [BingoItem] -> Bool
isRowBingo = all (==Marked)

isDiagBingo :: Board -> Bool
isDiagBingo = isRowBingo . diagonal

isBingo :: Board -> Bool
isBingo board =
    any isRowBingo board || any isRowBingo (transpose board) || isDiagBingo board || isDiagBingo (reverse board)

markItem :: Int -> BingoItem -> BingoItem
markItem n (Unmarked x) = if x == n then Marked else Unmarked x
markItem _ x = x

markBoard :: Int -> Board -> Board
markBoard n = map (map (markItem n))

playBingo :: [Int] -> [Board] -> (Int, Board)
playBingo [] _ = error "No winners"
playBingo (n:numbers) boards =
    let boards' = map (markBoard n) boards
    in case find isBingo boards' of
        Just b -> (n, b)
        Nothing -> playBingo numbers boards'

parseBoard :: [String] -> Board
parseBoard = map (map (Unmarked . read) . words)

parseBoards :: [String] -> [Board]
parseBoards xs
    | length xs < 6 = []
    | otherwise =
        let (boardInput, rem) = splitAt 6 xs
            board = parseBoard $ tail boardInput
        in board : parseBoards rem

parse :: String -> ([Int], [Board])
parse input =
    let inputLines = lines input
        numbers = map read $ splitOn "," $ head inputLines
        boards = parseBoards $ tail inputLines
    in (numbers, boards)

sumUnmarked :: Board -> Int
sumUnmarked b = sum $ map helper $ concat b
    where
        helper Marked = 0
        helper (Unmarked x) = x

getResult :: Int -> Board -> Int
getResult n board = n * sumUnmarked board

run :: String -> Int
run input =
    let (numbers, boards) = parse input
        (lastNumber, winningBoard) = playBingo numbers boards
    in getResult lastNumber winningBoard