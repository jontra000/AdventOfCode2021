module P15a where

import qualified Data.Map as M
import Data.Char (digitToInt)

type RiskMap = [[Int]]
type RouteRiskMap = [[Int]]

run :: String -> Int
run = result . parse

parse :: String -> RiskMap
parse = map (map digitToInt) . lines

result :: RiskMap -> Int
result riskMap = head (head (routeRisk riskMap)) - head (head riskMap)

routeRisk :: RiskMap -> RouteRiskMap
routeRisk [[a]] = [[a]]
routeRisk (as:bs) = 
    let subMap = map tail bs
        subRouteMap = routeRisk subMap
        columns = map head bs
        columns' = minimumRoute columns (map head subRouteMap)
        subRouteMap' = zipWith (:) columns' subRouteMap
        rows' = minimumRoute as (head subRouteMap')
    in  rows' : subRouteMap'
routeRisk _ = error "Non square map"

minimumRoute :: [Int] -> [Int] -> [Int]
minimumRoute risks routeRisks = init $ scanr minRoute (last routeRisks) (zip risks routeRisks)

minRoute :: (Int, Int) -> Int -> Int
minRoute (x, b) a = x + min a b