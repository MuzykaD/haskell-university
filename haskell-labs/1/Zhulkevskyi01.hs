{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi01 where

-- 1 -----------------------------------------
power3 :: [Integer]
power3 = [ x *x*x | x <- [1..] ]

--  2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3 ^ x|x<-[(1::Integer)..]]

--  3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3 ^ x|x<-[(1::Integer)..n]]

--  4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m ^ x|x<-[1..n]] 

--  5 -----------------------------------------
cntLess :: [Int] -> Int -> Int
cntLess xs x = length(filter (x>) xs)

lessMe :: [Int] -> [Int]
lessMe xs = map (cntLess xs) xs
--lessMe xs = [sum [if x > y then 1 else 0|y<-xs]|x<-xs]


--  6 -----------------------------------------
removeSimilar :: [Int] -> [Int]
removeSimilar [] = [] 
removeSimilar (x:xs) =  x:removeSimilar (filter (x /=) xs) 

freq :: [Int] -> Int -> Int
freq xs n
 | null xs = 0
 | head xs == n = 1+ freq (tail xs) n
 | otherwise = 0 +freq (tail xs) n

frequency :: [Int] -> [(Int,Int)]
frequency xs
 | null xs = []
 | otherwise = [(y, freq xs y)|y <- (removeSimilar xs)]

--  7 -----------------------------------------
hailstone :: Int -> Int
hailstone x | even x = div x 2
 | otherwise = x*3+1

--  8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq x | x == 1 = [1]
 | otherwise = x:hailSeq (hailstone x)

--  9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x| x<-[1..]]

--  10 -----------------------------------------
findNeededElement :: [[Int]] -> Int -> Int
findNeededElement xs x
 | null xs = -1
 | length (head xs) == x = 1
 | otherwise = 1+ findNeededElement (tail xs) x
 
firstHailSeq :: Int -> Int
firstHailSeq n = findNeededElement allHailSeq n