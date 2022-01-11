module P8a where

uniqueSegmentCount :: [String] -> Int
uniqueSegmentCount = length . filter (\x -> x < 5 || x == 7) . map length

segments :: String -> [String]
segments = words . tail . dropWhile (/= '|')

run :: String -> Int
run = uniqueSegmentCount . concatMap segments . lines