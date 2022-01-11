module P11a where

import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (findIndex)

data Point = Point Int Int deriving (Eq, Ord, Show)
data Octopus = Flashed | Charging Int deriving (Eq, Show)
type Cavern = M.Map Point Octopus

isFlashReady :: Octopus -> Bool
isFlashReady Flashed = False
isFlashReady (Charging x) = x > 9

charge :: Octopus -> Octopus
charge Flashed = Flashed
charge (Charging x) = Charging (x+1)

neighbours :: Point -> [Point]
neighbours (Point x y) = [Point (x-1) (y-1), Point x (y-1), Point (x+1) (y-1), Point (x-1) y, Point (x+1) y, Point (x-1) (y+1), Point x (y+1), Point (x+1) (y+1)]

chargePoints :: Cavern -> [Point] -> Cavern
chargePoints = foldr (M.adjust charge)

flashingPoints :: Cavern -> [Point]
flashingPoints = M.keys . M.filter isFlashReady

applyFlash :: Cavern -> Point -> Cavern
applyFlash cavern flashPoint = chargePoints cavern' $ neighbours flashPoint
    where
        cavern' = M.insert flashPoint Flashed cavern

processFlashes :: Cavern -> Cavern
processFlashes cavern
    | null flashingPoints' = cavern
    | otherwise = processFlashes $ applyFlash cavern p
    where
        flashingPoints' = flashingPoints cavern
        p = head flashingPoints'

resetOctopus :: Octopus -> Octopus
resetOctopus Flashed = Charging 0
resetOctopus x = x

resetAll :: Cavern -> Cavern
resetAll = M.map resetOctopus

iterator :: Cavern -> Cavern
iterator = processFlashes . M.map charge . resetAll

parsePoint :: Int -> Int -> Char -> (Point, Octopus)
parsePoint x y c = (Point x y, Charging (digitToInt c))

parseLine :: Int -> String -> [(Point, Octopus)]
parseLine y = zipWith (`parsePoint` y) [0..]

parse :: String -> Cavern
parse = M.fromList . concat . zipWith parseLine [0..] . lines

countFlashes :: Cavern -> Int
countFlashes = length . M.filter (==Flashed)

areFlashesSynchronised :: Cavern -> Bool
areFlashesSynchronised = all (==Flashed)

runA = sum . map countFlashes . take 101 . iterate iterator . parse

runB = findIndex areFlashesSynchronised . iterate iterator . parse

run = runB