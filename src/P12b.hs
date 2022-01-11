module P12b where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isUpper)

type CaveLinks = M.Map String (S.Set String)

cavesToVisit :: (Ord a, Ord k) => M.Map k (S.Set a) -> k -> Bool -> S.Set a -> [a]
cavesToVisit links currentCave False visitedCaves = S.toList linkedCaves
    where linkedCaves = links M.! currentCave
cavesToVisit links currentCave True visitedCaves = S.toList $ S.difference linkedCaves visitedCaves
    where linkedCaves = links M.! currentCave

pathsToExit :: CaveLinks -> S.Set String -> Bool -> String -> Int
pathsToExit _ _ _ "end" = 1
pathsToExit links visitedCaves smallCaveRepeated currentCave =
    let smallCaveRepeated' = smallCaveRepeated || S.member currentCave visitedCaves
        toVisit = cavesToVisit links currentCave smallCaveRepeated' visitedCaves
        visitedCaves' = if all isUpper currentCave then visitedCaves else S.insert currentCave visitedCaves
    in sum $ map (pathsToExit links visitedCaves' smallCaveRepeated') toVisit

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

removeStart :: CaveLinks -> CaveLinks
removeStart = M.map (S.delete "start")

linkMap :: [(String, String)] -> CaveLinks
linkMap = foldl addBidirectional M.empty

parse :: String -> CaveLinks
parse = removeStart . linkMap . map parseLine . lines

run input = pathsToExit (parse input) S.empty False "start"