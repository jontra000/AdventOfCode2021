module P7a where

import Data.List.Split (splitOn)
import Data.List (sort)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

getTarget :: [Int] -> Int
getTarget xs = sort xs !! (length xs `div` 2)

fuelUsed :: [Int] -> Int -> Int
fuelUsed start end = sum $ map (abs . (end -)) start

run :: String -> Int
run input =
    let initPositions = parseInput input
        targetPosition = getTarget initPositions
    in fuelUsed initPositions targetPosition