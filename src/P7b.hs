module P7b where

import Data.List.Split (splitOn)
import Data.List (sort)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

fuelUsed :: Int -> Int -> Int
fuelUsed end start = d * (d+1) `div` 2
    where d = abs (start - end)

totalFuelUsed :: [Int] -> Int -> Int
totalFuelUsed xs target = sum $ map (fuelUsed target) xs

run :: String -> Int
run input = minimum $ map (totalFuelUsed startPositions) range
    where 
        startPositions = parseInput input
        range = [minimum startPositions .. maximum startPositions]