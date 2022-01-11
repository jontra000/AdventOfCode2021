module P9b where
import qualified Data.Set as Set
import Data.List (sort)

data Point = Point Int Int deriving (Ord, Eq)

filterNot9Indices :: [(Int, Char)] -> [Int]
filterNot9Indices = map fst . filter ((/='9') . snd)

extractBasinPoints :: Int -> String -> [Point]
extractBasinPoints y = map (`Point` y) . filterNot9Indices . zip [0..]

parse :: String -> Set.Set Point
parse = Set.fromList . concat . zipWith extractBasinPoints [0..] . lines

countBasin :: Point -> Set.Set Point -> (Int, Set.Set Point)
countBasin p basins
    | Set.member p basins =
        let (c1, b1) = countBasin (Point (x-1) y) basins'
            (c2, b2) = countBasin (Point (x+1) y) b1
            (c3, b3) = countBasin (Point x (y-1)) b2
            (c4, b4) = countBasin (Point x (y+1)) b3
        in (1+c1+c2+c3+c4, b4)
    | otherwise = (0, basins)
    where
        basins' = Set.delete p basins
        Point x y = p
        countChild x' y' = countBasin (Point x' y') basins'

countBasins :: Set.Set Point -> [Int]
countBasins points
    | null points = []
    | otherwise =
        let (basinSize, points') = countBasin (Set.elemAt 0 points) points
        in basinSize : countBasins points'

result :: [Int] -> Int
result = product . take 3 . reverse . sort

run :: String -> Int
run = result . countBasins . parse