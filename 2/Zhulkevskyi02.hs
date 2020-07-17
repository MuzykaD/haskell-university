{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi02 where

-- 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs  = foldr (*) 1 xs

--  3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs


-- 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] n = n:[]
insert (x:xs) n | n<=x = n:x:xs
 | otherwise = x: insert xs n 
sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs

-- 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices f xs = [i |(x,i) <- zip xs [0..], f x] 

-- 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xs= reverse (map reverse xs)

-- 7  -----------------------------------------
noDigits :: String -> String
noDigits s= filter (\ch -> not (elem ch "0123456789")) s

-- 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood vs x = length $ filter ($ x) vs

-- 9 ------------------------------------------
trianglePas :: [[Integer]]
rowCreate :: [Integer] -> [Integer]
rowCreate xs = zipWith (+) (0:xs) (xs ++ [0])
trianglePas = iterate rowCreate [1]

-- 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : (zipWith (*) factorialsM [2..])

