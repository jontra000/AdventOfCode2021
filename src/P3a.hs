module P3a where
import Data.List (transpose)

run :: String -> Int
run input = gammaRate * espilonRate
    where
        inputLines = lines input
        bitCount = length $ head inputLines
        gammaRate = mostCommonBits inputLines
        espilonRate = 2^bitCount - 1 - gammaRate

mostCommonBits :: [String] -> Int
mostCommonBits input =
    let commonailityThreshold = length input `div` 2
        columns = transpose input
        columnBits = map (mostCommonBit commonailityThreshold) columns
    in parseBinary columnBits

mostCommonBit :: Int -> [Char] -> Bool 
mostCommonBit threshold = (> threshold) . length  . filter (== '1')

parseBinary :: [Bool] -> Int
parseBinary = foldl iterate 0
    where
        iterate acc False = acc*2
        iterate acc True = acc*2 + 1