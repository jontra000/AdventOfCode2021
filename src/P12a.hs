module P12a where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isUpper)

type CaveLinks = M.Map String (S.Set String)

pathsToExit :: CaveLinks -> S.Set String -> String -> Int
pathsToExit _ _ "end" = 1
pathsToExit links visitedCaves currentCave =
    let linkedCaves = links M.! currentCave
        toVisit = S.toList $ S.difference linkedCaves visitedCaves
        visitedCaves' = if all isUpper currentCave then visitedCaves else S.insert currentCave visitedCaves
    in sum $ map (pathsToExit links visitedCaves') toVisit

parseLine :: String -> (String, String)
parseLine s =
    let (a,b) = break (=='-') s
    in (a, tail b)

updateSet :: String -> Maybe (S.Set String) -> Maybe (S.Set String)
updateSet a Nothing = Just (S.singleton a)
updateSet a (Just s) = Just (S.insert a s)

addBidirectional :: CaveLinks -> (String, String) -> CaveLinks
addBidirectional links (a, b) =
    let links' = M.alter (updateSet b) a links
    in M.alter (updateSet a) b links' 

linkMap :: [(String, String)] -> CaveLinks
linkMap = foldl addBidirectional M.empty

parse :: String -> CaveLinks
parse = linkMap . map parseLine . lines

run input = pathsToExit (parse input) S.empty "start"