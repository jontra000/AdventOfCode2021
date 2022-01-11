module P20a where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
type Grid = M.Map Coord Bool
data State = State [Bool] Grid Bool

run :: String -> Int
run = solve 50 . parse

parse :: String -> ([Bool], Grid)
parse s =
    let l = lines s
        algo = map (=='#') (head l)
        grid = parseGrid (drop 2 l)
    in (algo, grid)

parseGrid :: [String] -> Grid
parseGrid = M.fromList . concat . zipWith parseGridRow [0..]

parseGridRow :: Int -> String -> [((Int, Int), Bool)]
parseGridRow y = zip (map (\x -> (x, y)) [0..]) . map (=='#')

solve :: Int -> ([Bool], Grid) -> Int
solve i (algo, grid) = litPixels $ iterate processImage (State algo grid False) !! i

processImage :: State -> State
processImage state@(State algo grid gridDefault) =
    let coords = nextImageRange (M.keys grid)
        grid' = M.fromList $ map (\c -> (c, iteratePoint state c)) coords
        gridDefault' = not gridDefault
    in State algo grid' gridDefault'

nextImageRange :: [Coord] -> [Coord]
nextImageRange coords =
    let xs = map fst coords
        ys = map snd coords
        xmin = minimum xs
        xmax = maximum xs
        ymin = minimum ys
        ymax = maximum ys
    in  [(x,y) | x <- [xmin - 1 .. xmax + 1], y <- [ymin - 1 .. ymax + 1]]

iteratePoint :: State -> Coord -> Bool
iteratePoint (State algo grid gridDefault) = (algo !!) . parseBinary . map (lookupPixel gridDefault grid) . surroundingCoords

surroundingCoords :: Coord -> [Coord]
surroundingCoords (x,y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

lookupPixel :: Bool -> Grid -> Coord -> Bool
lookupPixel gridDefault grid c = fromMaybe gridDefault (M.lookup c grid)

parseBinary :: [Bool] -> Int
parseBinary = foldl (\acc i -> acc*2 + i) 0 . map bool2Int
    where bool2Int False = 0
          bool2Int True = 1

litPixels :: State -> Int
litPixels = length . M.filter id . stateGrid

stateGrid :: State -> Grid
stateGrid (State _ grid _) = grid