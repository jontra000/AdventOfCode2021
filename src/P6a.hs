module P6a where

import Data.List.Split (splitOn)

childrenInDays :: [Int] -> [Int]
childrenInDays memo =
    let result
          | length memo <= 6 = 2
          | length memo <= 8 = 3
          | otherwise = memo !! 6 + memo !! 8
    in result : memo

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

run :: String -> Int
run input =
    let initFish = parseInput input
        spawningRates = iterate childrenInDays [] !! 256
    in sum (map (spawningRates !!) initFish)