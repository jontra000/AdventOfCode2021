module Main where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import P25a (run)

main :: IO ()
main = do
    ts1 <-  getCurrentTime
    input <- readFile "inputs/input25"
    let result = run input
    print result
    ts2 <- getCurrentTime
    print (utctDayTime ts2 - utctDayTime ts1)
