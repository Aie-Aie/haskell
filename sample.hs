combine :: [String] -> String
combine [] = []
combine (x:xs) = x ++ combine(xs)

doubleUs :: Int -> Int -> Int
doubleUs a b = a*2 + b*2

doubleMe :: Int -> Int
doubleMe a = a+a

doubleUsNew :: Int -> Int -> Int
doubleUsNew a b = doubleMe a + doubleMe b

reverseMe :: [Int] -> [Int]
reverseMe [] = []
reverseMe (x:xs) = reverseMe(xs) ++ [x]

rev :: [Int] -> [Int]
rev a = reverse a

--palindrome :: String -> Bool
--palindrome a = a == reverseMe(a)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

productS :: [Int] -> Int
productS [] = 1
productS (x:xs) = x * productS(xs)

qreverse :: [Int] -> [Int]
qreverse [] = []
qreverse a = reverseMe(quicksort(a))

factorial :: Int -> Int
factorial n = product[1..n]

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

average a = sum a `div` length a

stoi :: [Int] -> [Int] -> [Int]
stoi [] [] = []
stoi x y = [a*b | a <- x, b <- y]

bubblesort'iter :: (Ord a) => [a] -> [a]
bubblesort'iter (x:y:xs)
    | x > y = y : bubblesort'iter (x:xs)
    | otherwise = x : bubblesort'iter (y:xs)
bubblesort'iter (x) = (x)

bubblesort' :: (Ord a) => [a] -> Int -> [a]
bubblesort' xs i 
    | i == (length xs) = xs
    | otherwise = bubblesort' (bubblesort'iter xs) (i + 1) 
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' xs 0

extractUpper :: String -> String
extractUpper [] = []
extractUpper a = [x | x <- a, x `elem` ['A'..'Z']]

extractLower :: String -> String
extractLower [] = []
extractLower a = [x | x <- a, x `elem` ['a'..'z']]

abso :: Int -> Int
abso n = if n >= 0 then n else -n

signed :: Int -> Int
signed n = if n < 0 then -1
		else if n == 0 then 0 else 1

sign n | n < 0 = -1
	| n == 0 = 0
	| otherwise = 1

mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs

test :: [Char] -> Bool
test ('a':_) = True
test _ = False

rotate :: Int -> [Int] -> [Int]
rotate n xs = drop n xs ++ take n xs

percent :: Int -> Int -> Float
percent n m = ((fromInteger (toInteger n)) / (fromInteger (toInteger m))) * 100

inttofloat :: Int -> Float
inttofloat n = fromInteger (toInteger n)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs)
		| x > maxTail = x
		| otherwise = maxTail
		where maxTail = maximum' xs

maxy :: [Int] -> Int
maxy a = maximum a

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


factorial :: Integer -> Integer
factorial n = product [1..n]


add :: Integer -> Integer -> Integer
add a b = a+b

prod:: [Integer]  -> Integer
prod []=1
prod (x:xs) = x* prod xs

--Recursion on list
--(++) :function

--append operator
(#) ::[a] ->[a]->[a]
[] # ys = ys
ys # [] =ys
(x:xs) # ys = x: (xs # ys)

append :: [a]-> [a] -> [a]
append [] ys =ys
append ys [] =ys
append (x:xs) ys =x: append xs ys

--EX:
-- 1:2:3 :([] #[4 5])

rev :: [a] ->[a]
rev [] =[]
rev (x:xs) = rev xs # [x]
-- rev (x:xs) = apend (rev xs) [x]

zeroto :: Int -> [Int]
zeroto n =[0..n]

 
--perfect ::Int -> Bool
--perfect n =sum (init(factors n)) == n


insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    |x <= y = x:y:ys
    |otherwise = y: (insert x ys)

daeh :: [a] -> a
daeh [] = error "err"
daeh [x] =x
daeh (x:xs) = daeh xs

-- daeh xs =xs !! (length xs -1)

double :: Int -> Int
double n = n+n


clone :: Int -> a -> [a]
clone 0 _ = []
clone n x = x : (clone (n-1) x)
 
 
 --merge :: [a] -> [a] -> [a]
 