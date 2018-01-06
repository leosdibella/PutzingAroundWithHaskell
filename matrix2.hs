module Matrix'
	where

import Data.List

-- Single row subtraction (Duh!)
rsub :: [Double] -> [Double] -> [Double]
rsub [x] [y] = [(y - x)]
rsub (x:xs) (y:ys) = (y - x) : (rsub xs ys)

-- Row organization by pivots (0's in a certain postion are organized to be below rows with a pivot in that positon)
roworg :: Int -> [[Double]] -> [[Double]]
roworg y [] = []
roworg y (x:xs)
	| (x !! y) /= 0 = x : (roworg y xs)
	| ( (x !! y) == 0 && ([((xs !! k) !! y) | k<- [0..((length xs) - 1)], ((xs !! k) !! y) /= 0] == []) ) = (x:xs)
	| otherwise = (roworg y xs) ++ [x]

-- Row reduction to Echelon Form (1's in pivot positions, 0's below and to the left of each pivot position)
rowred :: [[Double]] -> [[Double]]
rowred xs = rowLoop 0 xs where
	rowLoop :: Int -> [[Double]] -> [[Double]]
	rowLoop y [] = []
	rowLoop y xs
		| ( y >= (length (xs !! 0)) ) = xs
		| ( length xs == 1 && [((xs !! 0) !! z) | z <- [0.. ((length (xs !! 0)) - 1)], ((xs !! 0) !! z) /= 0] == [] ) = xs
		| ( length xs == 1 ) = [ (map (*(1/(head [((xs!! 0) !! z) | z <- [0.. ((length (xs !! 0)) - 1)], ((xs !! 0) !! z) /= 0]))) (xs !! 0) ) ]
		| ( pvt == 0 ) = rowLoop (y+1) matrix
		| ( pvt /= 0 ) = ( fst : ( rowLoop (y+1) ( [ (rsub fst (map (*(1/ (z !! y))) z) ) | z <- (drop 1 xs) , (z !! y) /= 0 ]  ++ [ p | p <- xs , (p !! y) == 0 ] ) ) )
			where
			matrix = roworg y xs
			pvt = ((matrix !! 0) !! y) 
			fst = map (*(1/ pvt )) (matrix !! 0)

-- Row reduction to Reduced Echelon Form (1's in pivot positons, 0's above and below and to the left of each pivot positon)
redech :: [[Double]] -> [[Double]]
redech xs = redLoop ((length xs) - 1) ((length (xs !! 0)) - 1) (rowred xs) where
	redLoop :: Int -> Int -> [[Double]] -> [[Double]]
	redLoop y z [] = []
	redLoop y z ys
		| ( y == 0 || z == 0 ) = ys
		| ( [ ((ys !! y) !! p) | p <- [0..z] , ((ys !! y) !! p) == 1 , k <- [0..(z - 1)] , ((ys !! p) !! k) == 0 ] == [] ) = redLoop (y - 1) z ys
		| ( ( head [ p | p <- [0..z], ((ys !! y) !! p) == 1] )  /= z ) = redLoop y (head [ p | p <- [0..z], ((ys !! y) !! p) == 1]) ys
		| otherwise = redLoop (y - 1) (z - 1) (orgLoop z (ys !! z) ys) where
			orgLoop :: Int -> [Double] -> [[Double]] -> [[Double]]
			orgLoop s r [] = []
			orgLoop s r (t:ts) 
				| ( (t !! s) /= 0 && t /= r ) = ( rsub ( map (*(t !! s)) r)  t ) : (orgLoop s r ts)
				| otherwise = t : (orgLoop s r ts)