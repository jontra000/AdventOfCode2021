module P2a where

data Command = Forward Int | Up Int | Down Int

run :: String -> Int
run input = posHorizontal * posVertical
    where
        (posHorizontal, posVertical) = foldl iterate (0,0) $ map parse $ lines input
        iterate (h, v) (Forward x) = (h + x,v)
        iterate (h, v) (Up x) = (h, v - x)
        iterate (h, v) (Down x) = (h, v + x)

parse :: String -> Command
parse s =
    let (cmdStr:valStr:_) = words s
        val = read valStr
    in case cmdStr of
        "forward" -> Forward val
        "up" -> Up val
        "down" -> Down val
        _ -> error "bad input"
