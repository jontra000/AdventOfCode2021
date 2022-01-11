module P15b where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.List (minimumBy)
import Data.Function (on)
import qualified GHC.Base as Int
import Data.Tuple (swap)
import Data.Maybe (mapMaybe)

type Grid = [[Int]]

type Coord = (Int, Int)
type GridMap = M.Map Coord Int
type UnvisitedNode = (Int, Coord)
data UnvisitedNodes = UnvisitedNodes GridMap (S.Set UnvisitedNode)

-- run :: String -> Int
run = result . repeatMap . parse

parse :: String -> Grid
parse = map (map digitToInt) . lines

repeatMap :: Grid -> Grid
repeatMap = repeatX . repeatY

repeatY :: Grid -> Grid
repeatY = concat . take 5 . iterate incrementRisk

repeatX :: Grid -> Grid
repeatX = map (concat . take 5 . iterate incrementRiskRow)

incrementRisk :: Grid -> Grid
incrementRisk = map incrementRiskRow

incrementRiskRow :: [Int] -> [Int]
incrementRiskRow = map (wrap9 . (+1))

wrap9 :: Int -> Int
wrap9 10 = 1
wrap9 x = x

result :: Grid -> Int
result riskMap = dijkstra distances destination (initUnvisitedNodes distances) (0, (0,0))
    where distances = distanceMap riskMap
          destination = (length riskMap - 1, length riskMap - 1)

distanceMap :: Grid -> GridMap
distanceMap = M.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x,y), c)) [0..]) [0..]

initUnvisitedNodes :: GridMap -> UnvisitedNodes
initUnvisitedNodes riskMap =
    let initUnvisitedMap = M.map (const Int.maxInt) $ M.delete (0,0) riskMap
        initUnvisitedSet = S.fromList $ map swap $ M.toList initUnvisitedMap
    in  UnvisitedNodes initUnvisitedMap initUnvisitedSet

dijkstra :: GridMap -> Coord -> UnvisitedNodes -> UnvisitedNode -> Int
dijkstra distances destination unvisitedNodes (currentDistance, (x, y))
    | destination == (x,y) = currentDistance
    | otherwise  =
        let neighbours = S.fromList [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]
            (nextNode, unvisitedNodes') = updateNodes distances currentDistance unvisitedNodes neighbours
        in dijkstra distances destination unvisitedNodes' nextNode

updateNodes :: GridMap -> Int -> UnvisitedNodes -> S.Set Coord -> (UnvisitedNode, UnvisitedNodes)
updateNodes distances currentDistance (UnvisitedNodes pathMap pathCoords) toUpdate =
    let toUpdate' = lookupNodes pathMap toUpdate
        updatedNodes = M.mapWithKey (updateNode distances currentDistance) toUpdate'
        unvisitedNodes' = updateSet pathCoords updatedNodes
    in  updateNextNode unvisitedNodes' updatedNodes pathMap

updateNode :: GridMap -> Int -> Coord -> Int -> Int
updateNode distances currentDistance point oldDistance =
    let nextDistance = currentDistance + distances M.! point
    in min nextDistance oldDistance

updateNextNode :: S.Set UnvisitedNode -> GridMap -> GridMap -> (UnvisitedNode, UnvisitedNodes)
updateNextNode unvisitedNodes updatedNodes pathMap =
    let (nextNode, unvisitedNodes') = S.deleteFindMin unvisitedNodes
        pathMap' = M.delete (snd nextNode) $ M.union updatedNodes pathMap
    in (nextNode, UnvisitedNodes pathMap' unvisitedNodes')

updateSet :: S.Set UnvisitedNode -> GridMap -> S.Set UnvisitedNode
updateSet set updates =
    let updatesSet = S.fromList $ map swap $ M.toList updates
    in S.union updatesSet $ S.difference set updatesSet

lookupNodes :: GridMap -> S.Set Coord -> GridMap
lookupNodes paths = M.intersection paths . M.fromSet (const 0)