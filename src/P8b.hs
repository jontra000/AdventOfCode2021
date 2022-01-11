module P8b where
import Data.List (find, (\\), sort, elemIndex)
import Data.Char (chr, intToDigit)

findUnsafe :: (a -> Bool) -> [a] -> a
findUnsafe f xs =
    case find f xs of
        Just x -> x
        _ -> error "find failed"

indexUnsafe :: Eq a => [a] -> a -> Int
indexUnsafe xs x =
    case elemIndex x xs of
        Just x -> x
        _ -> error "index failed"

lengthIs :: Int -> [a] -> Bool
lengthIs n = (==n) . length

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll xs = all (`elem` xs)

find1 :: [String] -> String
find1 = findUnsafe (lengthIs 2)

find7 :: [String] -> String
find7 = findUnsafe (lengthIs 3)

find8 :: [String] -> String
find8 = findUnsafe (lengthIs 7)

find4 :: [String] -> String
find4 = findUnsafe (lengthIs 4)

find6 :: String -> [String] -> String
find6 str1 = findUnsafe (\x -> lengthIs 6 x && not (containsAll x str1))

find9 :: String -> [String] -> String
find9 str4 = findUnsafe (\x -> lengthIs 6 x && containsAll x str4)

find0 :: String -> String -> [String] -> String
find0 str6 str9 xs = findUnsafe (lengthIs 6) (xs \\ [str6, str9])

find5 :: String -> [String] -> String
find5 str6 = findUnsafe (\x -> lengthIs 5 x && containsAll str6 x)

find3 ::  String -> [String] -> String
find3 str1 = findUnsafe (\x -> lengthIs 5 x && containsAll x str1)

find2 :: String -> String -> [String] -> String
find2 str5 str3 xs = findUnsafe (lengthIs 5) (xs \\ [str5, str3])

patterns :: [String] -> [String]
patterns xs =
    let str1 = find1 xs
        str7 = find7 xs
        str8 = find8 xs
        str4 = find4 xs
        str6 = find6 str1 xs
        str9 = find9 str4 xs
        str0 = find0 str6 str9 xs
        str5 = find5 str6 xs
        str3 = find3 str1 xs
        str2 = find2 str5 str3 xs
    in map sort [str0, str1, str2, str3, str4, str5, str6, str7, str8, str9]

output :: [String] -> [String] -> Int
output digits = read . map (intToDigit . indexUnsafe digits . sort)

lineValue :: String -> Int
lineValue line =
    let (signalStr, valuesStr) = break (=='|') line
        digits = patterns (words signalStr)
        outputTokens = words (tail valuesStr)
    in output digits outputTokens

run :: String -> Int
run = sum . map lineValue . lines

