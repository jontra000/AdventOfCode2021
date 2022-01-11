module P1b where

run :: String -> Int
run xs = length positiveDeltas
    where
        is = map read $ lines xs
        deltas = zipWith (-) (drop 3 is) is
        positiveDeltas = filter (>0) deltas