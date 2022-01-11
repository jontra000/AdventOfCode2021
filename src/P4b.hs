module P4b where
import Data.List (transpose, find, nub)
import Data.List.Split (splitOn)
import Data.String (String)

type Board = [[Int]]

isRowBingo :: [Int] -> Bool
isRowBingo = null

isBingo :: Board -> Bool
isBingo = any isRowBingo

markBoard :: Int -> Board -> Board
markBoard n = map (filter (/= n))

playBingo :: [Int] -> [Board] -> (Int, Board)
playBingo [] _ = error "No winners"
playBingo (n:numbers) [lastBoard] =
    let board' = markBoard n lastBoard
    in if isBingo board' then (n, board') else playBingo numbers [board']
playBingo (n:numbers) boards =
    let boards' = filter (not . isBingo) $ map (markBoard n) boards
    in playBingo numbers boards'

parseBoard :: [String] -> Board
parseBoard lines =
    let grid = map (map read . words) lines
    in grid ++ transpose grid

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
sumUnmarked = sum . nub . concat

getResult :: Int -> Board -> Int
getResult n board = n * sumUnmarked board

-- run :: String -> Int
run input =
    let (numbers, boards) = parse input
        (lastNumber, losingBoard) = playBingo numbers boards
    in getResult lastNumber losingBoard