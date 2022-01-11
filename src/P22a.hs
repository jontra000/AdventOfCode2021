module P22a where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

data Range = Range Int Int
data Instruction = TurnOn (S.Set Point) | TurnOff (S.Set Point)
data Point = Point Int Int Int deriving (Eq, Ord)
type State = S.Set Point

run :: String -> Int
run = result . procedure . parse

parse :: String -> [Instruction]
parse = mapMaybe parseLine . lines

parseLine :: String -> Maybe Instruction
parseLine s =
    let (ins:ranges:_) = words s
    in instruction ins (parseRanges ranges)

parseRanges :: String -> (Maybe Range, Maybe Range, Maybe Range)
parseRanges s =
    let (x:y:z:_) = map parseRange $ splitOn "," s
    in (x,y,z)

parseRange :: String -> Maybe Range
parseRange s =
    let (start:end:_) = map read $ splitOn ".." $ drop 2 s
    in sanitiseRange $ Range start end

sanitiseRange :: Range -> Maybe Range
sanitiseRange (Range start end)
    | start > 50 = Nothing 
    | end < -50 = Nothing
    | otherwise = Just $ Range (max (-50) start) (min 50 end)

instruction :: String -> (Maybe Range, Maybe Range, Maybe Range) -> Maybe Instruction
instruction "on" (Just x, Just y, Just z) = Just $ TurnOn $ cuboid x y z
instruction "off" (Just x, Just y, Just z) = Just $ TurnOff $ cuboid x y z
instruction _ _ = Nothing

cuboid :: Range -> Range -> Range -> S.Set Point
cuboid (Range xStart xEnd) (Range yStart yEnd) (Range zStart zEnd) = S.fromList [Point x y z | x <- [xStart..xEnd], y <- [yStart..yEnd], z <- [zStart..zEnd]]

procedure :: [Instruction] -> State
procedure = foldl procedure' S.empty

procedure' :: State -> Instruction -> State
procedure' state (TurnOn points) = S.union state points
procedure' state (TurnOff points) = S.difference state points

result :: State -> Int
result = length