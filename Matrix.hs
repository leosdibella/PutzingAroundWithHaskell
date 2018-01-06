module Matrix
	where

import Data.List

-- Single row subtraction
rowsub :: [Double] -> [Double] -> [Double]
rowsub [x] [y] = [(y - x)]
rowsub (x:xs) (y:ys) = (y - x) : (rowsub xs ys)

-- Row organization by pivots
roworg :: Int -> [[Double]] -> [[Double]]
roworg i [] = []
roworg i [x] =[x]
roworg i (x:xs)
	| (x !! i) /= 0 = x : (roworg i xs)
	| ( (x !! i) == 0 && ([((xs !! k) !! i) | k<- [0..((length xs) - 1)], ((xs !! k) !! i) /= 0] == []) ) = (x:xs)
	| otherwise = (roworg i xs) ++ [x]

-- Row reduction to Echelon Form
rowred :: [[Double]] -> [[Double]]
rowred xs = rowLoop 0 xs where
	rowLoop :: Int -> [[Double]] -> [[Double]]
	rowLoop y [] = []
	rowLoop y [x] 
		| ( [(x !! y) | y <- [0.. ((length x) - 1)], (x !! y) /= 0] == [] ) = [x]
		| otherwise = [ (map (*(1/(head [(x !! y) | y <- [0.. ((length x) - 1)], (x !! y) /= 0]))) x) ]
	rowLoop y xs
		| (y > (length xs) ) = xs
		| ( ((matrix !! 0) !! y) == 0 ) = rowLoop (y+1) matrix
		| ( ((matrix !! 0) !! y) /= 0 && ((matrix !! 1) !! y) == 0 ) = ( fstred : ( rowLoop (y + 1) (drop 1 matrix) ) )
		| ( ((matrix !! 0) !! y) /= 0 && ((matrix !! 1) !! y) /= 0 ) = ( fstred : ( rowLoop (y+1) ( [ (rowsub fstred (map (*(1/ (z !! y))) z) ) | z <- (drop 1 xs) , (z !! y) /= 0 ]  ++ [ p | p <- xs , (p !! y) == 0 ] ) ) )
		| otherwise = ( fstred : (rowLoop (y+1) (roworg (y+1) (drop 1 matrix ) ) ) ) 
			where
			matrix = roworg y xs
			fstred = map (*(1/ ((matrix !! 0) !! y))) (matrix !! 0)