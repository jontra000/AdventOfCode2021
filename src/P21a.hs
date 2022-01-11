module P21a where
import Data.List.Split (chunksOf)
import Data.List (findIndex, find)
import Data.Maybe (fromJust)

data PlayerState = PlayerState Int Int deriving Show
data State = State PlayerState PlayerState deriving Show

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
solve = result . playToWin . playSeries . initState

initState :: (Int, Int) -> State
initState (position1, position2) = State (PlayerState position1 0) (PlayerState position2 0)

playSeries :: State -> [State]
playSeries initPositions = scanl playRound initPositions diceRolls

diceRolls :: [Int]
diceRolls = map sum $ chunksOf 3 $ concat $ repeat [1..100]

playRound :: State -> Int -> State
playRound (State (PlayerState position score) nextPlayer) diceRoll =
    let position' = modPosition (position + diceRoll)
    in State nextPlayer (PlayerState position' (position' + score))

modPosition :: Int -> Int
modPosition i = case i `mod` 10 of
    0 -> 10
    x -> x

playToWin :: [State] -> (Int, State)
playToWin = fromJust . find (isGameOver . snd) . zip [0,3..]

isGameOver :: State -> Bool
isGameOver (State _ (PlayerState _ score)) = score >= 1000

result :: (Int, State) -> Int
result (i, State (PlayerState _ score) _) = i * score