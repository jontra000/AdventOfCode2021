module P9a where
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)

isLowPoint :: Int -> [Int] -> Bool
isLowPoint 0 _ = True
isLowPoint 9 _ = False
isLowPoint x adjacent = all (> x) adjacent

lowPointValueAt :: [[Int]] -> Int -> Int -> Int
lowPointValueAt heightMap x y =
    let safeIndex xs x = if x < 0 || x >= length xs then Nothing else Just (xs !! x)
        lookupPoint x' y' = safeIndex heightMap y' >>= (`safeIndex` x')
        n = (heightMap !! y) !! x
        adjacent = catMaybes [lookupPoint (x-1) y, lookupPoint x (y-1), lookupPoint (x+1) y, lookupPoint x (y+1)]
    in if isLowPoint n adjacent then n+1 else 0

result :: [[Int]] -> Int
result heightMap =
    let yMax = length heightMap - 1
        xMax = length (head heightMap) - 1
    in sum $ concatMap (\x -> map (lowPointValueAt heightMap x) [0..yMax]) [0..xMax]

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

run :: String -> Int
run = result . parse