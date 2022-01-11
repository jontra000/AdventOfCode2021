module P10a where

errorScore :: [Char] -> String -> Int
errorScore _ [] = 0
errorScore ('(':chunkStack) (')':xs) = errorScore chunkStack xs
errorScore ('[':chunkStack) (']':xs) = errorScore chunkStack xs
errorScore ('{':chunkStack) ('}':xs) = errorScore chunkStack xs
errorScore ('<':chunkStack) ('>':xs) = errorScore chunkStack xs
errorScore _ (')':xs) = 3
errorScore _ (']':xs) = 57
errorScore _ ('}':xs) = 1197
errorScore _ ('>':xs) = 25137
errorScore chunkStack (x:xs) = errorScore (x:chunkStack) xs

run :: String -> Int
run = sum . map (errorScore []) . lines