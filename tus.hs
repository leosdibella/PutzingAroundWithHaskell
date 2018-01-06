module Maxerz
	where

import Data.List

-- Primality stuff

divis :: (Integral a) => a -> [a]
divis 0 = []
divis 1 = [1]
divis 2 = [1,2]
divis x = [r | r<- [1..x] , x `mod` r == 0]

prime :: (Integral a) => a -> Bool
prime x 
   | length (divis x) < 2 = False
   | length (divis x) == 2 = True
   | length (divis x) > 2 = False

primes :: (Integral a) => a -> [a]
primes 0 = []
primes 1 = []
primes 2 = [2]
primes x
   | prime x == True = x : primes(x - 1) 
   | otherwise = primes(x - 1)

--End Primality stuff

-- Random functions of interest

maxerz :: (Num a, Ord a) => a -> a -> a
maxerz x y 
	| x > y = x 
	| x == y = x*y
	| y > x = y

-- recursive function to find the maximum of a list

maximumz :: (Ord a) => [a] -> a
maximumz [] = error "You dumb"
maximumz [x] = x
maximumz (x:xs)
	| x > (maximumz xs) = x
	| otherwise = maximumz xs

-- tail recursive function to find the maximum of a list

maximumza :: (Ord a) => [a] -> a
maximumza [] = error "No maximum for empty lists"
maximumza (x:xs) = maxLoop xs x where
	maxLoop [] z = z
	maxLoop (y:ys) z 
		| y > z = maxLoop ys y
		| otherwise = maxLoop ys z

minimumza :: (Num a, Ord a) => [a] -> a
minimumza xs = negate $ maximumza $ map (negate) xs

-- recursive defintion of fibonacci

fibon :: Int -> Int
fibon 1 = 1
fibon 2 = 1
fibon n = ( fibon (n - 1) ) + ( fibon(n - 2) )

-- tail recursive defintion of fibonacci

fibontr :: Int -> Int
fibontr 1 = 1
fibontr 2 = 1
fibontr n = fibLoop 1 1 (n - 2) where
	fibLoop x y 0 = y
	fibLoop x y z = fibLoop y  (x + y)  (z - 1)

replicater ::  a -> Int -> [a]
replicater x y
	| y < 0 = error "Makes no sense"
	| y == 0 = []
	| y == 1 = [x]
	| otherwise = x : (replicater x (y -1))

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let  
	bottomsort  = quicksort [a | a <- xs, a <= x]
	topsort        = quicksort [a | a <- xs , a > x]
  in  bottomsort ++ [x] ++ topsort

splitList :: [a] -> ([a],[a])
splitList [] = ([],[])
splitList [x] = ([x],[])
splitList (x:y:zs) = let (xs,ys) = splitList zs in (x:xs, y:ys)  

merge :: (Ord a) => ([a],[a]) -> [a]
merge ([],[]) = []
merge ([],xs) = xs
merge (xs, []) = xs
merge (x:xs, y:ys)
	| x <= y = x : merge (xs,(y:ys))
	| otherwise = y : merge ((x:xs), ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (lower, upper) = splitList xs in merge ((mergesort lower),(mergesort upper))

-- Returns the last element of a list

tailz :: [a] -> a 
tailz [] = error "Empty lists have no elements"
tailz [x] = x
tailz (x:xs) = tailz xs

-- Returns the last specified elements of a list

tailzb :: Int -> [a] -> [a]
tailzb c [] = []
tailzb c xs
	| length xs <= c = xs
	| otherwise = take c $ reverse xs

-- Splits a list into equally sized sublists in sequential order

breakupb :: Int -> [a] -> [[a]]
breakupb c [] = [ [] ]
breakupb c xs
	| c >= (length xs) = [ xs ]
                  | otherwise = (take c xs) : (breakupb c $ drop c xs)

-- Checks if something is an element of a list

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
	| a == x    = True  
	| otherwise = a `elem'` xs   

-- Removes duplicate elements from lists
uniqueness :: (Ord a) => [a] -> [a]
uniqueness [] = []
uniqueness [x] = [x]
uniqueness (x:xs) 
	| elem' x xs = uniqueness xs
	| otherwise = x : (uniqueness xs)

-- Recursive Factorial
factorialz :: Integer -> Integer
factorialz 0 = 1
factorialz n = n*(factorialz (n - 1))

-- Tail Recursive Factorial
factorialza :: Integer -> Integer
factorialza 0 = 1
factorialza n = factloop n (n - 1) where
	factloop x y
		| x == 0 = y
		| otherwise = factloop (x - 2) (x*y) 

-- Removes all instances of something from a list
removelem :: (Ord a) => a -> [a] -> [a]
removelem x [] = error "Empty lists have no elements"
removelem x [y] 
	| x == y = []
	| otherwise = [y]
removelem x (y:ys) 
	| x == y = removelem x ys
	| otherwise = y : (removelem x ys)

-- Row swap
rowswap :: (Int, Int) -> [[Double]] -> [[Double]]
rowswap (i,j) [x] = [x]
rowswap (i,j) [] = []
rowswap (i,j) xs
	| i == j = xs
	| ( (i > length xs) || (j > length xs) ) = xs
	| ( (i < 0) || (j < 0) )= xs
	| i < j = [ (xs !! k) | k <- [0..(i-2)] ] ++ [ (xs !! (j - 1)) ] ++ [ (xs !! r) | r <- [i..(j - 2)] ] ++ [(xs !! (i - 1))] ++ [ (xs !! s ) | s <- [j.. ((length xs) - 1)] ]
	| j < i = rowswap (j,i) xs

-- Swap in
swapin :: Int -> a -> [a] -> [a]
swapin i x [] = []
swapin i x [y] 
	| (i == 1) = [x]
	| otherwise = [y]
swapin i x xs
	| ( (i > 0) && (i <= ( length xs ) ) ) = ( [(xs !! k) | k <- [0..(i - 2)] ] ++ [x] ++ [(xs !! k) | k <- [i..((length xs) - 1)] ] )
	| otherwise = error "What do you think you're doing?"

-- Listmake
listmake :: Integer -> [Integer]
listmake x
	| (x < 0) = error "what are you doing?"
	| (x >= 0) && (x < 10) = [x]
	| otherwise = y : listmake ( (x - y) `div` 10 ) where y = ( head [s | s <- [0..9] , (x - s) `mod` 10 == 0] )

-- Digits of an integer
digits :: Integer -> [Integer]
digits x = reverse . listmake  $ x

-- Digit sum
digitsum :: Integer -> Integer
digitsum x 
	| x < 0 = error "Not happening"
	| (x >= 0) && (x < 10) = x
	| otherwise = digitsum $ sum $ digits $ x

-- Binary Sum
bsum :: [Int] -> [Int] -> [Int]
bsum (x:xs) (y:ys) = bLoop 0 (x:xs) (y:ys) where
	bLoop :: Int -> [Int] -> [Int] -> [Int]
	bLoop n [] [] = []
	bLoop n [x] [y] 
		| (n == 1) && (x == 1) && (y == 1) = [1,1]
		| ( (n == 1) && ( ( (x == 1) && (y == 0) ) || ( (x == 0) && (y == 1) ) ) )
		 || ( (n == 0) && (x == 1) && (y == 1) ) = [0,1]
		| ( (n == 1) && (x == 0) && (y == 0) )
		 || ( (n == 0) && ( ( (x == 1) && (y ==0) ) || ( (x == 0) && (y == 1) ) ) ) = [1,0]
		| otherwise = [0,0]
	bLoop n (x:xs) (y:ys)
		| (n == 1) && (x == 1) && (y == 1) = 1: (bLoop 1 xs ys)
		| ( (n == 1) && ( ( (x == 1) && (y == 0) ) || ( (x == 0) && (y == 1) ) ) )
		 || ( (n == 0) && (x == 1) && (y == 1) ) = 0 : (bLoop 1 xs ys)
		| ( (n == 1) && (x == 0) && (y == 0) )
		 || ( (n == 0) && ( ( (x == 1) && (y ==0) ) || ( (x == 0) && (y == 1) ) ) ) = 1 : (bLoop 0 xs ys)
		| otherwise = 0 : (bLoop 0 xs ys)	

-- Better Binary Sum
bsumb :: [Int] -> [Int] -> [Int]
bsumb (x:xs) (y:ys) = bLoopb 0 (x:xs) (y:ys) where
	bLoopb :: Int -> [Int] -> [Int] -> [Int]
	bLoopb n [] []
		| (n == 1) = [1]
		| otherwise = [0]
	bLoopb n (x:xs) (y:ys)
		| (n == 1) && (x == 1) && (y == 1) = 1: (bLoopb 1 xs ys)
		| ( (n == 1) && ( ( (x == 1) && (y == 0) ) || ( (x == 0) && (y == 1) ) ) )
		 || ( (n == 0) && (x == 1) && (y == 1) ) = 0 : (bLoopb 1 xs ys)
		| ( (n == 1) && (x == 0) && (y == 0) )
		 || ( (n == 0) && ( ( (x == 1) && (y ==0) ) || ( (x == 0) && (y == 1) ) ) ) = 1 : (bLoopb 0 xs ys)
		| otherwise = 0 : (bLoopb 0 xs ys)	