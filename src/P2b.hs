module P2b where

data Command = Forward Integer  | Up Integer  | Down Integer 
data State = State {posHorizontal :: Integer, posVertical :: Integer, aim :: Integer}

run :: String -> Integer
run input = posHorizontal finalState * posVertical finalState
    where
        finalState = foldl iterate (State 0 0 0) $ map parse $ lines input
        iterate (State h v a) (Forward x) = State (h + x) (v + x * a) a
        iterate (State h v a) (Up x) = State h v (a - x)
        iterate (State h v a) (Down x) = State h v (a + x)

parse :: String -> Command
parse s =
    let (cmdStr:valStr:_) = words s
        val = read valStr
    in case cmdStr of
        "forward" -> Forward val
        "up" -> Up val
        "down" -> Down val
        _ -> error "bad input"