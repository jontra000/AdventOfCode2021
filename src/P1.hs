module P1 where

run :: String -> Int
run xs = length positiveDeltas
    where
        is = map read $ lines xs
        deltas = zipWith (-) (tail is) is
        positiveDeltas = filter (>0) deltas