module P21b where

import Data.List.Split (chunksOf)
import Data.List (findIndex, find, group, partition, sort, transpose, foldl1')
import Data.Maybe (fromJust)

data Player = Player1 | Player2 deriving (Eq, Show)
data PlayerState = PlayerState Player Int Int deriving Show
data State = State PlayerState PlayerState deriving Show
data Result = Result Int Int

run :: String -> Int
run = solve . parse

parse :: String -> (Int, Int)
parse s =
    let l = lines s
        position1 = parsePosition (head l)
        position2 = parsePosition (l !! 1)
    in (position1, position2)

parsePosition :: String -> Int
parsePosition = read . last . words

solve :: (Int, Int) -> Int
solve = result . playToWin . initState

initState :: (Int, Int) -> State
initState (position1, position2) = State (PlayerState Player1 position1 0) (PlayerState Player2 position2 0)

turnRolls :: [(Int, Int)]
turnRolls = countElems [a+b+c | a <- [1,2,3], b <- [1,2,3], c <- [1,2,3]]

countElems :: (Eq a, Ord a) => [a] -> [(Int, a)]
countElems = map (\l@(x:xs) -> (length l, x)) . group . sort

playRound :: State -> Int -> State
playRound (State (PlayerState player position score) nextPlayer) diceRoll =
    let position' = modPosition (position + diceRoll)
    in State nextPlayer (PlayerState player position' (position' + score))

modPosition :: Int -> Int
modPosition i = case i `mod` 10 of
    0 -> 10
    x -> x

playToWin :: State -> Result
playToWin state = mergeResults $ map (playToWin' state) turnRolls

playToWin' :: State -> (Int, Int) -> Result
playToWin' state (count, roll) = scale count result
    where state' = playRound state roll
          result = if isGameOver state' then winner state' else playToWin state'

isGameOver :: State -> Bool
isGameOver (State _ (PlayerState player _ score)) = score >= 21

winner :: State -> Result
winner (State _ (PlayerState Player1 _ _)) = Result 1 0
winner (State _ (PlayerState Player2 _ _)) = Result 0 1

mergeResults :: [Result] -> Result
mergeResults = foldl1' merge

merge :: Result -> Result -> Result
merge (Result a1 b1) (Result a2 b2) = Result (a1 + a2) (b1 + b2)

scale :: Int -> Result -> Result
scale i (Result a b) = Result (a*i) (b*i)

result :: Result -> Int
result (Result a b) = max a b