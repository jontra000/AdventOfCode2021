module P25a where

import qualified Data.Map as M
import Data.List (findIndex, elemIndex)
import Data.Maybe (catMaybes)

data Point = Point Int Int deriving (Eq, Ord, Show)
data State = Eastward | Southward deriving (Eq, Show)
type CucumberMap = M.Map Point State

-- run :: String -> Maybe Int
run = result . states . parse

debug x = showMap (x !! 1)

showMap cs = unlines $ map (\y -> map (\x -> showCuc (cs M.!? Point x y)) [0..9]) [0..8]

showCuc Nothing = '.'
showCuc (Just Eastward) = '>'
showCuc (Just Southward) = 'v'

parse :: String -> (Int, Int, CucumberMap)
parse input =
    let ls = lines input
        limitEast = length (head ls)
        limitSouth = length ls
        cucumbers = M.fromList $ concat $ zipWith parseLine [0..] ls
    in  (limitEast, limitSouth, cucumbers)

parseLine :: Int -> String -> [(Point, State)]
parseLine y = catMaybes . zipWith (parsePoint y) [0..]

parsePoint :: Int -> Int -> Char -> Maybe (Point, State)
parsePoint y x c = fmap (\state -> (Point x y, state)) (parseChar c)

parseChar :: Char -> Maybe State
parseChar '>' = Just Eastward
parseChar 'v' = Just Southward
parseChar _ = Nothing

states :: (Int, Int, CucumberMap) -> [CucumberMap]
states (limitEast, limitSouth, cucumbers) = iterate (cucumberStep limitEast limitSouth) cucumbers

cucumberStep :: Int -> Int -> CucumberMap -> CucumberMap
cucumberStep limitEast limitSouth = stepSouth limitSouth . stepEast limitEast

stepEast :: Int -> CucumberMap -> CucumberMap
stepEast limit = step Eastward (shiftPointEast limit)

stepSouth :: Int -> CucumberMap -> CucumberMap
stepSouth limit = step Southward (shiftPointSouth limit)

step :: State -> (Point -> Point) -> CucumberMap -> CucumberMap
step dir shiftPoint cucumbers =
    let toMove = M.filterWithKey (canMove dir shiftPoint cucumbers) cucumbers
    in  move dir shiftPoint cucumbers toMove

canMove :: State -> (Point -> Point) -> CucumberMap -> Point -> State -> Bool
canMove dir shiftPoint cucumbers p state = dir == state && not (M.member (shiftPoint p) cucumbers)

move :: State -> (Point -> Point) -> CucumberMap -> CucumberMap -> CucumberMap
move dir shiftPoint cucumbers toMove =
    let filledSpaces = M.mapKeys shiftPoint toMove
    in  M.union filledSpaces (M.difference cucumbers toMove)

shiftPointEast :: Int -> Point -> Point
shiftPointEast limit (Point x y) = Point ((x+1) `mod` limit) y

shiftPointSouth :: Int -> Point -> Point
shiftPointSouth limit (Point x y) = Point x ((y+1) `mod` limit)

result :: [CucumberMap] -> Maybe Int
result states = fmap (+1) (elemIndex True (zipWith (==) states (tail states)))
