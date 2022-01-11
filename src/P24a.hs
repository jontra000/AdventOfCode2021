module P24a where

import Data.List.Split (chunksOf)
import Data.List (sort)

data Processor = AddPoly Int | RemovePoly Int
data PolyPair = PolyPair Int Int Int

run :: String -> Int
run = solve . parse

parse :: String -> [Processor]
parse = map parseDigitProcessor . chunksOf 18 . lines

parseDigitProcessor :: [String] -> Processor
parseDigitProcessor lines =
    let divLine = lines !! 4
        addLine1 = lines !! 5
        addLine2 = lines !! 15
        offset1 = lastDigit addLine1
        offset2 = lastDigit addLine2
        divVal = lastDigit divLine
    in if divVal == 26 then RemovePoly offset1 else AddPoly offset2

lastDigit :: String -> Int
lastDigit = read . last . words

solve :: [Processor] -> Int
solve = solve' . pairPolys

pairPolys :: [Processor] -> [PolyPair]
pairPolys = snd . foldl pairPoly ([], []) . zip [13,12..]

pairPoly :: ([(Int, Int)], [PolyPair]) -> (Int, Processor) -> ([(Int, Int)], [PolyPair])
pairPoly (polys, acc) (i, AddPoly offset) = ((i, offset) : polys, acc)
pairPoly ((i, offset1) : polys, acc) (j, RemovePoly offset2) = (polys, PolyPair i j (offset1+offset2) : acc)

solve' :: [PolyPair] -> Int
solve' = digitsToInt . solve''

solve'' :: [PolyPair] -> [Int]
solve'' = map snd . sort . concatMap highestPolyPair

highestPolyPair :: PolyPair -> [(Int, Int)]
highestPolyPair (PolyPair i j diff) = if diff > 0 then [(i, 9 - diff), (j, 9)] else [(i, 9), (j, 9+diff)]

digitsToInt :: [Int] -> Int
digitsToInt = foldr (\x acc -> acc*10 + x) 0

-- inp w
-- mul x 0
-- add x z
-- mod x 26
-- div z 1
-- add x 14 : x <- 14
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25 
-- mul y x
-- add y 1 : y <- 26
-- mul z y
-- mul y 0
-- add y w
-- add y 7 : y <- i0 + 7
-- mul y x
-- add z y : z <- i0 + 7
-- inp w : w <- i1
-- mul x 0 : x <- 0
-- add x z : x <- i0 + 7
-- mod x 26 : x <- i0 + 7
-- div z 1
-- add x 12 : x <- i0 + 19
-- eql x w : x <- 0
-- eql x 0 : x <- 1
-- mul y 0 : y <- 0
-- add y 25 : y <- 25
-- mul y x : y <- 25
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26
-- mul y 0 : y <- 0
-- add y w : y <- i1
-- add y 4 : y <- i1 + 4
-- mul y x : y <- i1 + 4
-- add z y : z <- (i0+7)*26 + i1 + 4
-- inp w
-- mul x 0
-- add x z : x <- (i0+7)*26 + i1 + 4
-- mod x 26 : x <- i1 + 4
-- div z 1
-- add x 11 : x <- i1 + 15
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26*26 + (i1 + 4)*26
-- mul y 0
-- add y w
-- add y 8
-- mul y x : y <- i2 + 8
-- add z y : z <- (i0+7)*26*26 + (i1 + 4)*26 + i2 + 8
-- inp w
-- mul x 0
-- add x z : x <- (i0+7)*26*26 + (i1 + 4)*26 + i2 + 8
-- mod x 26 : x <- i2 + 8
-- div z 26 : z <- (i0+7)*26 + (i1+4)
-- add x -4 : x <- i2 + 4
-- eql x w : x <- if i2 + 4 == i3 then 1 else 0
-- eql x 0 : x <- if i2 + 4 == i3 then 0 else 1
-- mul y 0 : y <- 0
-- add y 25 : y <- 25
-- mul y x : y <- 0 | 25
-- add y 1 : y <- if i2 + 4 == i1 then 1 else 26
-- mul z y : z <- (i0+7)*26 + (i1+4) else (i0+7)*26*26 + (i1+4)*26
-- mul y 0 : y <- 0
-- add y w : y <- i3
-- add y 1 : y <- i3 + 1
-- mul y x : y <- 0 | i3 + 1
-- add z y : z <- (i0+7)*26 + (i1+4) | (i0+7)*26*26 + (i1+4)*26 + i3 + 1
-- inp w
-- mul x 0
-- add x z : x <- (i0+7)*26 + (i1+4)
-- mod x 26 : x <- i1+4
-- div z 1
-- add x 10 : x <- i1+14
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26*26 + (i1+4)*26
-- mul y 0
-- add y w
-- add y 5 : y <- i4+5
-- mul y x
-- add z y : z <- (i0+7)*26*26 + (i1+4)*26 + i4+5
-- inp w
-- mul x 0
-- add x z
-- mod x 26 : x <- i4+5
-- div z 1
-- add x 10 : x <- i4+15
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26
-- mul y 0
-- add y w
-- add y 14
-- mul y x : y <- i5+14
-- add z y : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26 + i5+14
-- inp w
-- mul x 0
-- add x z
-- mod x 26 : x <- i5+14
-- div z 1
-- add x 15 : x <- i5+29
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26*26*26*26 + (i1+4)*26*26*26 + (i4+5)*26*26 + (i5+14)*26
-- mul y 0
-- add y w
-- add y 12 : y <- i6+12
-- mul y x
-- add z y : z <- (i0+7)*26*26*26*26 + (i1+4)*26*26*26 + (i4+5)*26*26 + (i5+14)*26 + i6+12
-- inp w
-- mul x 0
-- add x z
-- mod x 26 : x <- i6+12
-- div z 26 : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26 + i5+14
-- add x -9 : x <- i6+3
-- eql x w
-- eql x 0 : x <- if i6+3 == i7 then 0
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1
-- mul z y
-- mul y 0
-- add y w
-- add y 10
-- mul y x : y <- 0
-- add z y : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26 + i5+14
-- inp w
-- mul x 0
-- add x z
-- mod x 26 : x <- i5+14
-- div z 26 : z <- (i0+7)*26*26 + (i1+4)*26 + (i4+5)
-- add x -9 : x <- i5 + 5
-- eql x w
-- eql x 0 : 0 (i5 + 5 == i8)
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1
-- mul z y
-- mul y 0
-- add y w
-- add y 5
-- mul y x
-- add z y : z <- (i0+7)*26*26 + (i1+4)*26 + (i4+5)
-- inp w
-- mul x 0
-- add x z
-- mod x 26 : x <- i4+5
-- div z 1
-- add x 12 : x <- i4+17
-- eql x w
-- eql x 0 : x <- 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 26
-- mul z y : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26
-- mul y 0
-- add y w
-- add y 7
-- mul y x : y <- i9 + 7
-- add z y : z <- (i0+7)*26*26*26 + (i1+4)*26*26 + (i4+5)*26 + i9+7
-- inp w : w <- i10
-- mul x 0
-- add x z
-- mod x 26 : x <- i9+7
-- div z 26 : z <- (i0+7)*26*26 + (i1+4)*26 + (i4+5)
-- add x -15 : x <- i9-8
-- eql x w
-- eql x 0 : x <- 0 (i9-8 == i10)
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1
-- mul z y : z <- (i0+7)*26*26 + (i1+4)*26 + (i4+5)
-- mul y 0
-- add y w
-- add y 6
-- mul y x : y <- 0
-- add z y : z <- (i0+7)*26*26 + (i1+4)*26 + (i4+5)
-- inp w : w <- i11
-- mul x 0
-- add x z
-- mod x 26 : x <- i4+5
-- div z 26 : z <- (i0+7)*26 + (i1+4)
-- add x -7 : x <- i4-2
-- eql x w
-- eql x 0 : x <- 0 (i4-2 == i11)
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1
-- mul z y : z <- (i0+7)*26 + (i1+4)
-- mul y 0
-- add y w
-- add y 8
-- mul y x : y <- 0
-- add z y : z <- (i0+7)*26 + (i1+4)
-- inp w : w <- i12
-- mul x 0
-- add x z
-- mod x 26 : i1 + 4
-- div z 26 : z <- (i0+7)
-- add x -10 : i1 - 6
-- eql x w
-- eql x 0 : x <- 0 (i1 - 6 == i12)
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1
-- mul z y : z <- (i0+7)
-- mul y 0
-- add y w
-- add y 4
-- mul y x : y <- 0
-- add z y : z <- i0+7
-- inp w : w <- i13
-- mul x 0
-- add x z
-- mod x 26 : x <- 0
-- div z 26 : z <- 0
-- add x 0
-- eql x w
-- eql x 0 : if i0+7==i13 then 0 else 1
-- mul y 0
-- add y 25
-- mul y x
-- add y 1 : y <- 1 | 26
-- mul z y : z <- 0
-- mul y 0
-- add y w
-- add y 6 
-- mul y x : y <- 0 | i13+6
-- add z y : z <- i0+7 | i0+7 + i13 + 6