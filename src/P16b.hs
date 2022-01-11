module P16b where
import Data.Char (digitToInt)

data Packet = Literal Int Int | Operator Int [Packet]

run :: String -> Int
run = parseSinglePacket . toBinary

toBinary :: String -> [Int]
toBinary = concatMap toBinaryChar

toBinaryChar :: Char -> [Int]
toBinaryChar c =
    let i = digitToInt c
    in reverse $ take 4 $ map (`mod` 2) $ iterate (`div` 2) i

parseSinglePacket :: [Int] -> Int
parseSinglePacket s = fst $ parsePacket s

parseMultiplePackets :: [Int] -> [Int]
parseMultiplePackets s
    | length s < 6 = []
    | otherwise = 
        let (packet, s'') = parsePacket s
        in  packet : parseMultiplePackets s''

parseMultiplePacketsCount :: Int -> [Int] -> ([Int], [Int])
parseMultiplePacketsCount 0 s = ([], s)
parseMultiplePacketsCount n s =
    let (packet, s') = parsePacket s
        (ps, s'') = parseMultiplePacketsCount (n-1) s'
    in (packet : ps, s'')

parsePacket :: [Int] -> (Int, [Int])
parsePacket s = 
    let (header, s') = splitAt 6 s
        version = fromBinary $ take 3 header
        packetType = fromBinary $ drop 3 header
    in  parsePacket' version packetType s'
    
parsePacket' :: Int -> Int -> [Int] -> (Int, [Int])
parsePacket' version 4 s = parseLiteral s 0
parsePacket' version o s =
    let (contents, s') = parseOperator s
    in (performOperator o contents, s')

performOperator :: Int -> [Int] -> Int
performOperator 0 = sum
performOperator 1 = product 
performOperator 2 = minimum
performOperator 3 = maximum
performOperator 5 = operatorPair (>)
performOperator 6 = operatorPair (<)
performOperator 7 = operatorPair (==)

operatorPair :: (Int -> Int -> Bool) -> [Int] -> Int
operatorPair f [a,b] = if f a b then 1 else 0
operatorPair _ _ = error "More than 2 packets for binary operator"

parseLiteral :: [Int] -> Int -> (Int, [Int])
parseLiteral (c:s) acc =
    let (vs, s') = splitAt 4 s
        v = acc*16 + fromBinary vs
    in if c == 0 then (v, s') else parseLiteral s' v

parseOperator :: [Int] -> ([Int], [Int])
parseOperator (0:s) = parseOperatorChars s    
parseOperator (_:s) = parseOperatorPackets s

parseOperatorChars :: [Int] -> ([Int], [Int])
parseOperatorChars s =
    let (lStr, s') = splitAt 15 s
        l = fromBinary lStr
        (content, s'') = splitAt l s'
    in (parseMultiplePackets content, s'')

parseOperatorPackets :: [Int] -> ([Int], [Int])
parseOperatorPackets s =
    let (lStr, s') = splitAt 11 s
        l = fromBinary lStr
    in  parseMultiplePacketsCount l s'

fromBinary :: [Int] -> Int
fromBinary = foldl folder 0
    where folder acc i = acc*2 + i