module P5a where
import Data.List (group, sort, elemIndices)
import Data.Char (chr, intToDigit)

data Point = Point Int Int deriving (Eq, Ord)

count :: Ord a => [a] -> [(a, Int)]
count =  map (\l@(x:xs) -> (x,length l)) . group . sort

getResult :: [[Point]] -> Int
getResult = length . filter ((> 1) . snd) . count . concat

parsePoint :: String -> Point
parsePoint str =
    let (xStr, yStr) = break (==',') str
    in Point (read xStr) (read $ tail yStr)

verticalLine :: Int -> Int -> Int -> [Point]
verticalLine x yStart yEnd =
    let start = min yStart yEnd
        end = max yStart yEnd
    in map (Point x) [start..end]

horizontalLine :: Int -> Int -> Int -> [Point]
horizontalLine y xStart xEnd =
    let start = min xStart xEnd
        end = max xStart xEnd
    in map (`Point` y) [start..end]

diagonalLine :: Int -> Int -> Int -> Int -> [Point]
diagonalLine x1 y1 x2 y2 =
    let xDir = signum (x2 - x1)
        yDir = signum (y2 - y1)
    in zipWith Point [x1,x1+xDir..x2] [y1,y1+yDir..y2]

linePoints :: Point -> Point -> [Point]
linePoints (Point xStart yStart) (Point xEnd yEnd)
    | xStart == xEnd = verticalLine xStart yStart yEnd
    | yStart == yEnd = horizontalLine yStart xStart xEnd
    | otherwise = []

parseLine :: String -> [Point]
parseLine input =
    let (startStr:_:endStr:_) = words input
        start = parsePoint startStr
        end = parsePoint endStr
    in linePoints start end

parsePoints :: String -> [[Point]]
parsePoints = map parseLine . lines

run :: String -> Int
run = getResult . parsePoints