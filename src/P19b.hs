module P19b where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)

data Coord = Coord Int Int Int deriving (Eq, Ord)
type Beacons = S.Set Coord
type Scanners = S.Set Coord
data Space = Space Beacons Scanners
data Translation = Translation Int Int Int
type Rotation = Coord -> Coord
data Transform = Transform Translation Rotation

run :: String -> Int
run = result . parse

parse :: String -> [Space]
parse = map parseScanner . splitOn [""] . lines

parseScanner :: [String] -> Space
parseScanner = newSpace . S.fromList . map parseCoord . tail

parseCoord :: String -> Coord
parseCoord s =
    let (x:y:z:_) = map read $ splitOn "," s
    in Coord x y z

newSpace :: Beacons -> Space
newSpace beacons = Space beacons (S.singleton (Coord 0 0 0))

result :: [Space] -> Int
result xs =
    let (Space _ scanners) = solve (head xs) (tail xs)
    in largestManhattanDistance scanners

solve :: Space -> [Space] -> Space
solve a xs =
    let (a', xs') = solve' a xs
    in if null xs' then a' else solve a' xs'

solve' :: Space -> [Space] -> (Space, [Space])
solve' a [] = (a, [])
solve' a (x:xs) = case combineSpaces a x of
    Just a' -> (a', xs)
    Nothing ->
        let (a', xs') = solve' a xs
        in (a', x:xs')

combineSpaces :: Space -> Space -> Maybe Space
combineSpaces s1 s2 = result $ mapMaybe (matchSpaces s1 s2) rotations
    where result [] = Nothing
          result (s':_) = Just s'

matchSpaces :: Space -> Space -> Rotation -> Maybe Space
matchSpaces s1 s2 rotation =
    let s2' = applyRotation s2 rotation
        s2'' = find (spaceMatches s1) (translatedSets s1 s2')
    in fmap (mergeSpaces s1) s2''

mergeSpaces :: Space -> Space -> Space
mergeSpaces (Space b1 s1) (Space b2 s2) = Space (S.union b1 b2) (S.union s1 s2)

spaceMatches :: Space -> Space -> Bool
spaceMatches space1@(Space b1 s1) (Space b2 s2) =
    all (beaconMatches space1) b2 && length (S.intersection b1 b2) >= 12

beaconMatches :: Space -> Coord -> Bool
beaconMatches (Space beacons scanners) c = S.member c beacons || isOutOfRange scanners c

isOutOfRange :: Scanners -> Coord -> Bool
isOutOfRange scanners (Coord x y z) = all (\(Coord x' y' z') -> abs (x - x') > 1000 || abs (y - y') > 1000 || abs (z -z') > 1000) scanners

applyRotation :: Space -> Rotation -> Space
applyRotation (Space beacons scanners) rotation = Space (S.map rotation beacons) (S.map rotation scanners)

translatedSets :: Space -> Space -> [Space]
translatedSets (Space b1 s1) space2@(Space b2 s2) = [applyTranslation space2 (translation a b) | a <- S.toList b1, b <- S.toList b2]

applyTranslation :: Space -> Translation -> Space
applyTranslation (Space beacons scanners) t = Space (S.map (translate t) beacons) (S.map (translate t) scanners)

translate :: Translation -> Coord -> Coord
translate (Translation dx dy dz) (Coord x y z) = Coord (x+dx) (y+dy) (z+dz)

translation :: Coord -> Coord -> Translation
translation (Coord ax ay az) (Coord bx by bz) = Translation (ax-bx) (ay-by) (az-bz)

rotations :: [Rotation]
rotations =
    let r1 = [
            id,
            \(Coord x y z) -> Coord (-z) y x,
            \(Coord x y z) -> Coord z y (-x),
            \(Coord x y z) -> Coord y (-x) z,
            \(Coord x y z) -> Coord (-y) x z,
            \(Coord x y z) -> Coord (-x) y (-z)]
        r2 = [
            id,
            \(Coord x y z) -> Coord x z (-y),
            \(Coord x y z) -> Coord x (-y) (-z),
            \(Coord x y z) -> Coord x (-z) y
            ]
    in [b . a | a <- r1, b <- r2]

largestManhattanDistance :: Scanners -> Int
largestManhattanDistance scanners = maximum [manhattanDistance a b | a <- scanners', b <- scanners']
    where scanners' = S.toList scanners

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (Coord x1 y1 z1) (Coord x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)