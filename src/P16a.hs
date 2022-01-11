module P16a where
import Data.Char (digitToInt)

data Packet = Literal Int Int | Operator Int [Packet]

run :: String -> Int
run = sumVersionNumbers . fst . parsePackets (-1) . toBinary

toBinary :: String -> [Int]
toBinary = concatMap toBinaryChar

toBinaryChar :: Char -> [Int]
toBinaryChar c =
    let i = digitToInt c
    in reverse $ take 4 $ map (`mod` 2) $ iterate (`div` 2) i

parsePackets :: Int -> [Int] -> ([Packet], [Int])
parsePackets 0 s = ([], s)
parsePackets i s
    | length s < 6 = ([], [])
    | otherwise = parsePackets' i s

parsePackets' :: Int -> [Int] -> ([Packet], [Int])
parsePackets' i s =
    let (header, s') = splitAt 6 s
        version = fromBinary $ take 3 header
        packetType = fromBinary $ drop 3 header
        (p, s'') = parsePacket version packetType s'
        (ps, s''') = parsePackets (i-1) s''
    in  (p:ps, s''')

parsePacket :: Int -> Int -> [Int] -> (Packet, [Int])
parsePacket version 4 s =
    let (value, s') = parseLiteral s 0
    in (Literal version value, s')
parsePacket version _ s =
    let (contents, s') = parseOperator s
    in (Operator version contents, s')

parseLiteral :: [Int] -> Int -> (Int, [Int])
parseLiteral (c:s) acc =
    let (vs, s') = splitAt 4 s
        v = acc*16 + fromBinary vs
    in if c == 0 then (v, s') else parseLiteral s' v

parseOperator :: [Int] -> ([Packet], [Int])
parseOperator (0:s) = parseOperatorChars s    
parseOperator (_:s) = parseOperatorPackets s

parseOperatorChars :: [Int] -> ([Packet], [Int])
parseOperatorChars s =
    let (lStr, s') = splitAt 15 s
        l = fromBinary lStr
        (content, s'') = splitAt l s'
    in (fst $ parsePackets (-1) content, s'')

parseOperatorPackets :: [Int] -> ([Packet], [Int])
parseOperatorPackets s =
    let (lStr, s') = splitAt 11 s
        l = fromBinary lStr
    in  parsePackets l s'

fromBinary :: [Int] -> Int
fromBinary = foldl folder 0
    where folder acc i = acc*2 + i

sumVersionNumbers :: [Packet] -> Int
sumVersionNumbers = sum . map sumVersionNumbers'

sumVersionNumbers' :: Packet -> Int
sumVersionNumbers' (Literal v _) = v
sumVersionNumbers' (Operator v ps) = v + sumVersionNumbers ps