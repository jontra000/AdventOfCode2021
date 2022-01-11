module P18a where
import Data.Complex (magnitude)
import Data.Char (isNumber)
import Data.Maybe (fromMaybe)

data SnailNumber = RegularNumber Int | NumberPair SnailNumber SnailNumber deriving (Eq, Show)

run :: String -> Int
run = snailMagnitude . snailSum . parse

parse :: String -> [SnailNumber]
parse = map (fst . parseNumber) . lines

parseNumber :: String -> (SnailNumber, String)
parseNumber ('[':xs) =
    let (left, ',':xs') = parseNumber xs
        (right, xs'') = parseNumber xs'
    in (NumberPair left right, tail xs'')
parseNumber xs =
    let (n, xs') = span isNumber xs
    in (RegularNumber (read n), xs')

snailSum :: [SnailNumber] -> SnailNumber
snailSum = foldl1 snailAdd

snailAdd :: SnailNumber -> SnailNumber -> SnailNumber
snailAdd a b = snailReduce (snailAdd' a b)

snailAdd' :: SnailNumber -> SnailNumber -> SnailNumber
snailAdd' = NumberPair

snailReduce :: SnailNumber -> SnailNumber
snailReduce x =
    let x' = snailReduce' x
    in if x == x' then x' else snailReduce x'

snailReduce' :: SnailNumber -> SnailNumber
snailReduce' x =
    let x' = explode x
    in fromMaybe x' (split x')

explode :: SnailNumber -> SnailNumber
explode x =
    let (x', _, _) = explode' 0 x
    in x'

explode' :: Int -> SnailNumber -> (SnailNumber, Int, Int)
explode' 4 (NumberPair (RegularNumber left) (RegularNumber right)) = (RegularNumber 0, left, right)
explode' depth (NumberPair left right) =
    let (left', expLeft1, expRight1) = explode' (depth+1) left
        (right', expLeft2, expRight2) = explode' (depth+1) (incrementLeft right expRight1)
        left'' = incrementRight left' expLeft2
    in  (NumberPair left'' right', expLeft1, expRight2)
explode' _ x = (x, 0, 0)

incrementLeft :: SnailNumber -> Int -> SnailNumber
incrementLeft (RegularNumber x) a = RegularNumber (x+a)
incrementLeft (NumberPair left right) x = NumberPair (incrementLeft left x) right

incrementRight :: SnailNumber -> Int -> SnailNumber
incrementRight (RegularNumber x) a = RegularNumber (x+a)
incrementRight (NumberPair left right) x = NumberPair left (incrementRight right x)

split :: SnailNumber -> Maybe SnailNumber
split n@(RegularNumber x) = if x > 9 then Just (NumberPair (RegularNumber x') (RegularNumber (x - x'))) else Nothing
    where x' = x `div` 2
split (NumberPair left right) =
    case split left of
        Just left' -> Just (NumberPair left' right)
        Nothing -> case split right of
            Just right' -> Just (NumberPair left right')
            Nothing -> Nothing

snailMagnitude :: SnailNumber -> Int
snailMagnitude (RegularNumber x) = x
snailMagnitude (NumberPair left right) = 3*snailMagnitude left + 2*snailMagnitude right