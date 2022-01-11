module P22b where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe (catMaybes)

data Range = Range Int Int deriving Show
data Instruction = TurnOn Cuboid | TurnOff Cuboid
data Point = Point Int Int Int deriving (Eq, Ord)
data Cuboid = Cuboid Range Range Range deriving Show
type State = [Cuboid]

-- run :: String -> Int
run = result . procedure . parse

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine s =
    let (ins:ranges:_) = words s
    in instruction ins (parseRanges ranges)

parseRanges :: String -> (Range, Range, Range)
parseRanges s =
    let (x:y:z:_) = map parseRange $ splitOn "," s
    in (x,y,z)

parseRange :: String -> Range
parseRange s =
    let (start:end:_) = map read $ splitOn ".." $ drop 2 s
    in Range start end

instruction :: String -> (Range, Range, Range) -> Instruction
instruction "on" (x, y, z) = TurnOn $ Cuboid x y z
instruction "off" (x, y, z) = TurnOff $ Cuboid x y z
instruction i _ = error ("bad instruction " ++ i)

procedure :: [Instruction] -> State
procedure = foldl procedure' []

procedure' :: State -> Instruction -> State
procedure' state (TurnOn cuboid) = cuboid : concatMap (cutCuboid cuboid) state
procedure' state (TurnOff cuboid) = concatMap (cutCuboid cuboid) state

cutCuboid :: Cuboid -> Cuboid -> [Cuboid]
cutCuboid cuboid2 cuboid1 = if cuboidIntersects cuboid1 cuboid2
    then catMaybes [leftCuboid cuboid1 cuboid2, bottomCuboid cuboid1 cuboid2, topCuboid cuboid1 cuboid2,
        frontCuboid cuboid1 cuboid2, backCuboid cuboid1 cuboid2, rightCuboid cuboid1 cuboid2]
    else [cuboid1]

cuboidIntersects :: Cuboid -> Cuboid -> Bool
cuboidIntersects (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = rangeIntersects x1 x2 && rangeIntersects y1 y2 && rangeIntersects z1 z2

rangeIntersects :: Range -> Range -> Bool
rangeIntersects (Range start1 end1) (Range start2 end2) = end1 >= start2 && end2 >= start1

leftCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
leftCuboid (Cuboid (Range xStart1 _) y1 z1) (Cuboid (Range xStart2 _) _ _) =
    if xStart1 < xStart2 then Just $ Cuboid (Range xStart1 (xStart2-1)) y1 z1 else Nothing

rightCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
rightCuboid (Cuboid (Range _ xEnd1) y1 z1) (Cuboid (Range _ xEnd2) _ _) =
    if xEnd1 > xEnd2 then Just $ Cuboid (Range (xEnd2+1) xEnd1) y1 z1 else Nothing

bottomCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
bottomCuboid (Cuboid x1 (Range yStart1 _) z1) (Cuboid x2 (Range yStart2 _) _) =
    if yStart2 > yStart1 then Just $ Cuboid (middleRange x1 x2) (Range yStart1 (yStart2-1)) z1 else Nothing

topCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
topCuboid (Cuboid x1 (Range _ yEnd1) z1) (Cuboid x2 (Range _ yEnd2) _) =
    if yEnd1 > yEnd2 then Just $ Cuboid (middleRange x1 x2) (Range (yEnd2+1) yEnd1) z1 else Nothing

frontCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
frontCuboid (Cuboid x1 y1 (Range zStart1 _)) (Cuboid x2 y2 (Range zStart2 _)) =
    if zStart2 > zStart1 then Just $ Cuboid (middleRange x1 x2) (middleRange y1 y2) (Range zStart1 (zStart2-1)) else Nothing

backCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
backCuboid (Cuboid x1 y1 (Range _ zEnd1)) (Cuboid x2 y2 (Range _ zEnd2)) =
    if zEnd1 > zEnd2 then Just $ Cuboid (middleRange x1 x2) (middleRange y1 y2) (Range (zEnd2+1) zEnd1) else Nothing

middleRange :: Range -> Range -> Range
middleRange (Range start1 end1) (Range start2 end2) = Range (max start1 start2) (min end1 end2)

result :: State -> Int
result = sum . map cuboidSize

cuboidSize :: Cuboid -> Int
cuboidSize (Cuboid x y z) = rangeLength x * rangeLength y * rangeLength z

rangeLength :: Range -> Int
rangeLength (Range start end) = end - start + 1