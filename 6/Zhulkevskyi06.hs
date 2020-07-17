{-# OPTIONS_GHC -Wall #-}
module ZhulkevskyiP06 where
import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

edgeIn :: Graph -> (Int,Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

edges::Graph -> [(Int,Int)]
edges g = [(x,y)| x<-nodes g, y <-g!!x]

nodes :: Graph -> [Int]
nodes g = [0..(length g -1)]

goNodes :: Graph -> Int -> [Int]
goNodes gr v = sort $ snd $ until cond (oneStep gr) ([v],[])

cond :: ([Int],[Int]) -> Bool
cond (new,_) = null new

oneStep :: Graph -> ([Int], [Int]) -> ([Int],[Int]) 
oneStep gr (ns,os) = let old = ns ++os
                         ns1 = concatMap (gr !!) ns
                         ns2 = filter (`notElem` old) ns1
                         new = nub ns2
 in (new,old)

allWays:: Graph -> Int -> [[[Int]]] 
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: [[[Int]]] -> Bool
condW ways = null (head ways)
stepW :: Graph -> [[[Int]]]->[[[Int]]]
stepW gr wss = let wsn = head wss
                   wsn1 = continueWay gr (filter (\(x:xs)->notElem x xs) wsn)
               in wsn1:wss 

continueWay :: Graph ->[[Int]]->[[Int]]
continueWay gr wsn = concatMap (\xs@(x:_)-> map (\y -> y:xs) (gr !! x)) wsn
-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = checkVertexes gr (zip gr [0..])

checkVertexes :: Graph ->[([Int],Int)] -> Bool
checkVertexes _ [] = True
checkVertexes gr ((xs,i):xss)
 | isConnections gr xs i= checkVertexes gr xss 
 | otherwise = False
isConnections :: Graph -> [Int] -> Int -> Bool 
isConnections  _ [] _= True
isConnections gr (x:xs) i
 | i == x = False
 | elem x xs = False
 | elem i (gr !! x) = isConnections gr xs i
 | otherwise = False
-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr= (length gr - 1,edges gr)

-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph 
toGraph (i,xs) = foldl addVertex (replicate (i+1) [])xs

addVertex :: Graph -> (Int,Int)->Graph
addVertex gr (x,y) = let (ys,zs) = splitAt x gr
 in ys ++ [(head zs++[y])] ++ tail zs

-- Задача 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr from to = case waysFromTo gr from to of
 [] -> []
 xs -> reverse $ last xs 
 
waysFromTo :: Graph -> Int ->Int -> [[Int]]
waysFromTo gr from to = filter (\x -> to == head x) (concat $ allWays gr from)
-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = length gr == (length $ goNodes gr 0)

-- Задача 6 ------------------------------------
components :: Graph -> [[Int]] 
components gr= reverse $ snd $ until condComp (findComponent gr) ([0..(length gr-1)],[])
condComp :: ([Int],[[Int]])->Bool
condComp (vs,_) = null vs 
findComponent :: Graph -> ([Int],[[Int]]) -> ([Int],[[Int]])
findComponent gr (xs,res) = let comp = goNodes gr (head xs) 
 in (filter (\x-> notElem x comp) xs,comp:res) 

-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v= maximum $ map (\x -> (length $ shortWay gr v x) -1) [0..(length gr-1)]

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr= maximum $ map (eccentricity gr) [0..(length gr-1)]

findRadius :: Graph -> Int 
findRadius gr = minimum $ map (eccentricity gr) [0..(length gr-1)]

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr= let rad = findRadius gr 
 in filter (\x-> rad == eccentricity gr x) [0..(length gr-1)]

-- Задача 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr from to = case waysFromTo gr from to of
    [] -> []
    xs -> map (reverse) (filter (\x-> (length $ last xs) == (length x) ) xs) 

----------------------------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]