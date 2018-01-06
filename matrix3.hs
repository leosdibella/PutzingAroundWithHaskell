module Matrix''
	where

import Data.List

-- Single row subtraction (Duh!)
rsub :: [Double] -> [Double] -> [Double]
rsub [x] [y] = [(y - x)]
rsub (x:xs) (y:ys) = (y - x) : (rsub xs ys)

-- Comparison of Double arrays which is needed to sort pivot positions
rowEq :: [Double] -> [Double] -> Ordering
[] `rowEq` [] = EQ
(x:xs) `rowEq` (y:ys)
	| (x /= 0 && y /= 0) || (x == 0 && y == 0) = xs `rowEq` ys
	| x /= 0 && y == 0 = GT
	| otherwise = LT

-- Variation of quicksort to sort the pivot positions from "largest" to "smallest" (use mergesort for HUGE matrices or whatever)
msort :: [[Double]] -> [[Double]]
msort [] = []
msort (x:xs) = let  
	bottomsort  = msort [a | a <- xs, (a `rowEq` x == EQ ||  a `rowEq` x == GT) ]
	topsort        = msort [a | a <- xs, (a `rowEq` x == LT) ]
	in  bottomsort ++ [x] ++ topsort

-- Row reduction algorthim which puts any matrix into row reduced echelon form
rowred :: [[Double]] -> [[Double]]
rowred xs = redLoop 0 0 (msort xs) where
	redLoop :: Int -> Int -> [[Double]] -> [[Double]]
	redLoop i j [] = []
	redLoop i j ys 
		|  j >= ncol || i >= nrow = ys
		| pvt == 0 = redLoop i (j + 1) ys
		| otherwise = redLoop (i + 1) (j + 1) $ msort $ [ rsub (map (* (((ys !! p) !! j)  / pvt) ) (ys !! i)) (ys !! p) | p <- ( [0..(i-1)] 
						    ++ [(i+1)..(nrow - 1)] ) , ((ys !! p) !! j) /= 0 ] 
						    ++ [ map (* (1 / pvt) ) (ys !! i) ] ++ [ ys !! p | p <- ( [0..(i-1)] 
						    ++ [ (i+1)..(nrow - 1)] ) , ((ys !! p) !! j) == 0 ] 
			where
			pvt = ((ys !! i) !! j) 
			ncol = length $ ys !! 0 
			nrow = length ys

-- Basis vector builder 
vbuild :: Int -> Int -> [Double]
vbuild n i = vLoop n i n where
	vLoop :: Int -> Int -> Int -> [Double]
	vLoop n i 0 = []
	vLoop n i m 
		| (m == i) = 1 : ( vLoop n i (m - 1) )
		| otherwise = 0 : ( vLoop n i (m - 1) )

-- Identity matrix builder 
idbuild :: Int -> [[Double]]
idbuild n = idLoop n n where
	idLoop :: Int -> Int -> [[Double]]
	idLoop x y 
		| y < 1 = []
		| otherwise = ( vbuild x y ) : ( idLoop x (y -1) )
 
-- Matrix Inverse Calculator FOR REALZ
minv :: [[Double]] -> [[Double]]
minv xs = zipWith (drop) (replicate (length xs) (length xs) ) ( rowred $ zipWith (++) xs (idbuild (length xs)) )