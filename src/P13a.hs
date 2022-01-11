module P13a where
import Data.List (nub)
import qualified Data.Set as S

data Point = Point Int Int deriving (Eq, Ord)

data Fold = XFold Int | YFold Int

parsePoint :: String -> Point
parsePoint str =
    let (xStr, yStr) = break (==',') str
    in Point (read xStr) (read $ tail yStr)

parseFold :: String -> Fold
parseFold str =
    let payloadStr = words str !! 2
        val = read (drop 2 payloadStr)
    in case head payloadStr of
        'x' -> XFold val
        'y' -> YFold val
        c -> error ("Unexpected fold char " ++ show c)

parse :: String -> ([Point], [Fold])
parse input =
    let (pointStrs, foldStrs) = break (== "") (lines input)
    in (map parsePoint pointStrs, map parseFold (tail foldStrs))

foldX :: Int -> Point -> Point
foldX foldVal (Point x y)
    | foldVal < x = Point (foldVal - (x - foldVal)) y
    | otherwise = Point x y

foldY :: Int -> Point -> Point
foldY foldVal (Point x y)
    | foldVal < y = Point x (foldVal - (y - foldVal))
    | otherwise = Point x y

foldPaper :: [Point] -> Fold -> [Point]
foldPaper ps (XFold xFold) = map (foldX xFold) ps
foldPaper ps (YFold yFold) = map (foldY yFold) ps

showPoint :: S.Set Point -> Int -> Int -> Char
showPoint points y x
    | S.member (Point x y) points = '*'
    | otherwise = '.'

showPoints :: [Point] -> String
showPoints points =
    let maxX = maximum $ map (\(Point x _) -> x) points
        maxY = maximum $ map (\(Point _ y) -> y) points
        pointsSet = S.fromList points
        output = map (\y -> map (showPoint pointsSet y) [0..maxX]) [0..maxY]
    in unlines output

runA :: String -> Int
runA input =
    let (points, folds) = parse input
        points' = foldPaper points (head folds)
    in length $ nub points'

runB :: String -> String
runB input =
    let (points, folds) = parse input
        points' = foldl foldPaper points folds
    in showPoints points'

run = runB

-- ***..*..*.*..*.****.****..**..*..*.***.\n
-- *..*.*.*..*..*.*.......*.*..*.*..*.*..*\n
-- *..*.**...****.***....*..*....*..*.***.\n
-- ***..*.*..*..*.*.....*...*.**.*..*.*..*\n
-- *.*..*.*..*..*.*....*....*..*.*..*.*..*\n
-- *..*.*..*.*..*.*....****..***..**..***.\n