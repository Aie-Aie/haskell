
import Data.Char (ord, chr)
type Bit = Int

--BASE CONVERSION
bin2int :: [Bit] -> Int
bin2int bits = sum[w*b | (w, b) <- zip weights bits]
    where weights = iterate(*2)1

--bin2int[1, 0, 1,1]
 
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin m = (m `mod` 2): int2bin(m `div` 2)
--int2bin 13

make8 :: [Bit] -> [Bit]
make8 bits = take 8(bits ++ repeat 0)
--make8 [1, 0, 1, 1]

encode :: String -> [Bit]
encode = concat . map(make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

------------------------------------------------------REVISIONS
addparity :: [Bit] -> [Bit]
addparity xs
    | even (sum xs) = xs ++ [0]
    | otherwise = xs ++ [1]

--addparity [1, 0, 1, 1]  == [1, 0, 1,1,1]
encode' :: String -> [Bit]
encode' = concat . map (addparity . make8 . int2bin .ord)

paritychecker :: [Bit] -> Bool
paritychecker xs
    | length xs == 9 && ((sum (init xs)) `mod` 2) == (last xs) = True
    | otherwise = error "decode error"

discardparitybit :: [[Bit]] -> [[Bit]]
discardparitybit xs = map (take 8) xs


decode' :: [Bit] -> String
decode' xs = map (chr . bin2int) . discardparitybit $ filter paritychecker (chop8 xs)

---
discarder :: Int -> [a] -> [a]
discarder n xs = map snd (filter (\(index, _) -> index `mod` n /= 0) $ zip [0..] xs)

channel' :: [Bit] -> [Bit]
channel' = discarder 9

transmit' :: String -> String
transmit' = decode' . channel' . encode'

