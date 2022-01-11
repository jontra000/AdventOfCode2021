module P14b where

import qualified Data.Map as M
import Data.List (group, sort, sortBy)

type InsertionRules = M.Map (Char, Char) Char
type ElemCount = M.Map Char Int
data MemoKey = MemoKey Int Char Char deriving (Eq, Ord)
type Memo = M.Map MemoKey ElemCount

parseTemplate :: [String] -> String
parseTemplate = head

parseInsertion :: String -> ((Char, Char), Char)
parseInsertion s = ((head s, s !! 1), last s)

parseInsertions :: [String] -> InsertionRules
parseInsertions = M.fromList . map parseInsertion . drop 2

mergeElemCounts :: ElemCount -> ElemCount -> ElemCount
mergeElemCounts = M.unionWith (+)

mergeMultipleElemCounts :: [ElemCount] -> ElemCount
mergeMultipleElemCounts = foldl mergeElemCounts M.empty

applyInsertions :: InsertionRules -> Memo -> Int -> Char -> Char -> Memo
applyInsertions rules memo 0 a b = M.insert k (M.singleton a 1) memo
    where k = MemoKey 0 a b
applyInsertions rules memo i a b =
    let c = rules M.! (a,b)
        r1 = applyInsertionsMemo rules memo (i-1) a c
        r2 = applyInsertionsMemo rules r1 (i-1) c b
        lookupResult x y = r2 M.! MemoKey (i-1) x y
        result = mergeElemCounts (lookupResult a c) (lookupResult c b)
    in  M.insert (MemoKey i a b) result r2

applyInsertionsMemo :: InsertionRules -> Memo -> Int -> Char -> Char -> Memo
applyInsertionsMemo rules memo i a b = maybe (applyInsertions rules memo i a b) (const memo) (M.lookup k memo)
    where k = MemoKey i a b

generatePolymer :: InsertionRules -> Int -> String -> ElemCount
generatePolymer rules i input =
    let memo = foldl folder M.empty (zip input (tail input))
    in mergeMultipleElemCounts (results memo)
    where folder memo (a, b) = applyInsertionsMemo rules memo i a b
          resultKeys = zipWith (MemoKey i) input (tail input)
          results memo = map (memo M.!) resultKeys ++ [M.singleton (last input) 1]

countElems :: (Eq a, Ord a) => [a] -> [(Int, a)]
countElems = map (\l@(x:xs) -> (length l, x)) . group . sort

result :: ElemCount -> Int
result polymer = maximum xs - minimum xs
    where xs = M.elems polymer

run :: String -> Int
run input =
    let l = lines input
        start = parseTemplate l
        rules = parseInsertions l
        end = generatePolymer rules 40 start
    in result end