module P17a where
import Data.Char (isNumber)
import Data.List.Split (splitOn)
import Data.List (findIndex)

type Target = ((Int, Int), (Int, Int))

run :: String -> Int
run = findMaxY . parse

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

findMaxY :: Target -> Int
findMaxY ((xMin, xMax), (yMin, yMax)) = sum [1..(-yMin-1)]