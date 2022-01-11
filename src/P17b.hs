module P17b where

import Data.Char (isNumber)
import Data.List.Split (splitOn)
import Data.List (findIndex, find)

type Target = ((Int, Int), (Int, Int))

run :: String -> Int
run = successfulVelocities . parse

parse :: String -> Target
parse s =
    let s' = dropWhile (not . isNumber) s
        (xStr, s'') = break (==',') s'
        yStr = dropWhile (/= '-') s''
    in  (parseCoord xStr, parseCoord yStr)

parseCoord :: String -> (Int, Int)
parseCoord s =
    let (minStr:maxStr:_) = splitOn ".." s
    in (read minStr, read maxStr)

successfulVelocities :: Target -> Int
successfulVelocities target@((xMin, xMax), (yMin, _)) = length $ filter (hitsTarget target) velocities
    where (Just minXVelocity) = find ((>= xMin) . triangularNumber) [1..]
          velocities = [(x,y)| x <- [minXVelocity..xMax], y <- [yMin..(-yMin)]]

hitsTarget :: Target -> (Int, Int) -> Bool
hitsTarget target@((_, xmax), (ymin, _)) (x, y) =
    any (inTarget target) $ takeWhile (mayStillHitTarget xmax ymin) $ trajectory x y

mayStillHitTarget :: Int -> Int -> (Int, Int) -> Bool
mayStillHitTarget xmax ymin (x, y) = y >= ymin && x <= xmax

inTarget :: Target -> (Int, Int) -> Bool
inTarget ((xmin, xmax), (ymin, ymax)) (x, y) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

trajectory :: Int -> Int -> [(Int, Int)]
trajectory x y = zip (scanl (+) 0 $ xVelocity x) (scanl (+) 0 $ yVelocity y)

xVelocity :: Int -> [Int]
xVelocity x = [x, x-1 .. 0] ++ repeat 0

yVelocity :: Int -> [Int]
yVelocity y = [y, y-1 ..]

triangularNumber :: Int -> Int
triangularNumber n = n*(n+1) `div` 2