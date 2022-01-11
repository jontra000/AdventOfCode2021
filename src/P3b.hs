module P3b where
import Data.List (transpose)

run input = oxygenRate * co2Rate
    where
        inputLines = lines input
        oxygenRate = parseBinary $ filterBitCriteria '1' 0 inputLines
        co2Rate = parseBinary $ filterBitCriteria '0' 0 inputLines

mostCommonBit :: Int -> [Char] -> Bool 
mostCommonBit threshold = (>= threshold) . length  . filter (== '1')

parseBinary :: String-> Int
parseBinary = foldl iterate 0
    where
        iterate acc '0' = acc*2
        iterate acc '1' = acc*2 + 1
        iterate _ _ = error "unexpected char"

otherChar :: Char -> Char
otherChar '0' = '1'
otherChar '1' = '0'
otherChar _ = error "unexpected character"

matchesBit :: Char -> Int -> String -> Bool
matchesBit bit i = (==bit) . (!! i)

matches :: Char -> Int -> [String] -> [String]
matches matchBit i = filter (matchesBit matchBit i)

matchingBit :: Char -> Int -> [Char] -> Char
matchingBit defaultChar l bits =
    let threshold = l `div` 2 + l `mod` 2
        bitPositive = mostCommonBit threshold bits
    in if bitPositive then defaultChar else otherChar defaultChar

filterBitCriteria :: Char -> Int -> [String] -> String
filterBitCriteria _ _ [r] = r
filterBitCriteria defaultChar i xs = 
    let bits = map (!! i) xs
        l = length xs
        matchBit = matchingBit defaultChar l bits
        xs' = matches matchBit i xs
    in filterBitCriteria defaultChar (i+1) xs'