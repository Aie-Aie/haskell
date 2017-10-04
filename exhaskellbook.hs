-- arithmetic operations 

-- 2+15
--49 * 100
-- 1892 -1472
-- 5/2

--Boolean operations
-- &&
-- ||
-- not True


--Functions in Haskell
-- succ 8 == 9
-- min 9 10 == 9
--min 3.4 3.3 == 3.2
-- max 100 101 == 101
-- succ 9 + max 5 4 + 1 ==16
-- succ (9*10) ==91


--List Manipulation
-- [1, 2, 3, 4] ++ [5, 6, 7] == [1, 2, 3, 4, 5, 6, 7]
-- head [1, 2, 3, 4] =1
-- tail [5, 4, 2, 3] = [4, 2, 3]
-- last [5, 3, 2, 1] = 1
-- init [5, 2, 3, 1] = [5, 2, 3]
-- length[5, 4, 2, 3]= 4
-- null [1, 2, 3]   ==  True or False
-- reverse [1, 2, 3] = [3, 2, 1]
-- take 3 [5, 3, 4, 2, 1] =[5, 3, 4]
-- take 0 = []
-- drop 3 [8, 2, 4, 1, 2] =[1, 2]
-- minimum [1, 2, 3, 4, 5] =1
-- maximum [1, 2, 3, 4, 5] =5
-- sum [1, 2, 3, 4, 5] = 1+2+3+4+5
-- product [6, 2, 1, 2] = 24
-- 4 'elem' [3, 4, 5, 6] = True  :elem- return T or F if the element is in the list
-- [1 .. 20] ==[1, 2, 3.. , 20]
-- [a .. z] ==[a, b, c, .. x]
-- [3, 6, .. 20] == [3, 6, 9, 12, ... 18]
-- cycle ([1, 2, 3]) === infinite list
-- repeat (5)  == infinite list
-- [x*2 | x <- [1 .. 10]] == [2, 4, 6, 8, 9 ,10, ..20]

-- Tuple Manipulation
-- fst(8, 11) == 8 takes a pair and return its first component
--snd (8, 11) == 11
-- t fst

--Equality
-- 5/= 5 (not equal)-> Bool
-- read "5" :: Int == 5

doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

--factorial :: Integer -> Integer
--factorial n = product[1 .. n]

factorial :: (Integral a) => a -> a
factorial 0 =1
factorial n =n* factorial (n-1)


circumference :: Float -> Float
circumference n =2 * pi * n

head' :: [a] -> a
head' []= error "Can't tell"
head' (x:_) =x

length' ::[a] -> Int
length' [] =0
length' (_:xs) = 1+ length' xs

sum' :: Num(a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: Float -> String
bmiTell bmi
    | bmi <= 18.5 = "UW"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "OW"
    | otherwise = "Whale"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b =a
    | otherwise = b

cylinder :: Float -> Float -> Float
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea