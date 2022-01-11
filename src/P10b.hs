module P10b where
import Data.List (sort)

openChunks :: [Char] -> String -> [Char]
openChunks chunkStack [] = chunkStack
openChunks ('(':chunkStack) (')':xs) = openChunks chunkStack xs
openChunks ('[':chunkStack) (']':xs) = openChunks chunkStack xs
openChunks ('{':chunkStack) ('}':xs) = openChunks chunkStack xs
openChunks ('<':chunkStack) ('>':xs) = openChunks chunkStack xs
openChunks _ (')':xs) = []
openChunks _ (']':xs) = []
openChunks _ ('}':xs) = []
openChunks _ ('>':xs) = []
openChunks chunkStack (x:xs) = openChunks (x:chunkStack) xs

filterCorrupted :: [[Char]] -> [[Char]]
filterCorrupted = filter (not . null)

scoreCompletionString :: [Char] -> Int 
scoreCompletionString = foldl updateScore 0 . map charScore
    where 
        updateScore acc c = 5*acc + c
        charScore '(' = 1
        charScore '[' = 2
        charScore '{' = 3
        charScore '<' = 4
        charScore c = error ("Unexpected char " ++ show c)

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

run :: String -> Int
run = median . map scoreCompletionString . filterCorrupted . map (openChunks []) . lines
