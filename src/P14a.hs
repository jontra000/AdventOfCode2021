module P14a where

import qualified Data.Map as M
import Data.List (group, sort, sortBy)

type InsertionRules = M.Map (Char, Char) Char

parseTemplate :: [String] -> String
parseTemplate = head

parseInsertion :: String -> ((Char, Char), Char)
parseInsertion s = ((head s, s !! 1), last s)

parseInsertions :: [String] -> InsertionRules
parseInsertions = M.fromList . map parseInsertion . drop 2

update :: InsertionRules -> (Char, Char) -> String
update rules p = [fst p, rules M.! p]

step :: InsertionRules -> String -> String
step rules input = concatMap (update rules) (zip input (tail input)) ++ [last input]

countElems :: (Eq a, Ord a) => [a] -> [(Int, a)]
countElems = map (\l@(x:xs) -> (length l, x)) . group . sort

result :: String -> Int
result polymer =
    let elementCounts = sort (map fst (countElems polymer))
    in last elementCounts - head elementCounts

run :: String -> Int
run input =
    let l = lines input
        start = parseTemplate l
        rules = parseInsertions l
        end = iterate (step rules) start !! 10
    in result end