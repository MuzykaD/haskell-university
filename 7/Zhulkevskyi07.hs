{-# OPTIONS_GHC -Wall #-}
module ZhulkevskyiP07 where

import Data.List
--BTree - firstly balance, then insert
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- ������ 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch (EmptyM) = True
isSearch (NodeM v k tl tr)
 | (k>0) && (checkLeftBinaryElements tl v) && (checkRightBinaryElements tr v) = isSearch tl && isSearch tr
 | otherwise = False

checkLeftBinaryElements:: (Ord a) => BinTreeM a -> a -> Bool
checkLeftBinaryElements tree v= and $ map (\x->v>x) (findBinaryElements tree)

checkRightBinaryElements:: (Ord a) => BinTreeM a -> a -> Bool
checkRightBinaryElements tree v= and $ map (\x->v<x) (findBinaryElements tree)

findBinaryElements :: (Ord a) => BinTreeM a -> [a]
findBinaryElements EmptyM = []
findBinaryElements (NodeM v _ tl tr) = v:(findBinaryElements tl) ++ findBinaryElements tr 
-- ������ 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch (NodeM v _ tl tr) el 
 | v==el = True
 | el>v = elemSearch tr el
 | otherwise = elemSearch tl el
elemSearch (EmptyM) _ = False     

-- ������ 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch (NodeM v k tl tr) el 
 | v==el = NodeM v (k+1) tl tr
 | el>v = NodeM v k tl (insSearch tr el)
 | otherwise = NodeM v k (insSearch tl el) tr
insSearch (EmptyM) el = NodeM el 1 EmptyM EmptyM

-- ������ 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch (NodeM v k tl tr) el 
 | v==el = case (tl,tr) of
               (NodeM _ _ _ _, NodeM _ _ _ _) -> case findMostLeft tr of 
                     Just (a,b) -> NodeM a b tl (delSearch tr a)
                     Nothing -> tr
               (tl1@(NodeM _ _ _ _), EmptyM) -> tl1
               (EmptyM, tr1@(NodeM _ _ _ _)) -> tr1
               (EmptyM, EmptyM) -> EmptyM
 | el>v = NodeM v k tl (delSearch tr el)
 | otherwise = NodeM v k (delSearch tl el) tr
delSearch (EmptyM) _ = EmptyM

findMostLeft:: (Ord a) => BinTreeM a -> Maybe (a,Int)
findMostLeft (NodeM _ _ tl@(NodeM _ _ _ _) _) = findMostLeft tl
findMostLeft (NodeM v k (EmptyM) _) =  Just (v,k) 
findMostLeft (EmptyM) = Nothing

-- ������ 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList list = sortTree (foldl insSearch EmptyM list)

sortTree::(Ord a)=> BinTreeM a -> [a]
sortTree (NodeM v k tl tr ) = sortTree tl ++(replicate k v)++sortTree tr
sortTree (EmptyM) = []

-- ������ 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform tree= BInform {hB = (levelBTree tree), minB = (mostLeft tree), maxB= (mostRight tree)}

levelBTree :: (Bounded a, Ord a) => Btree a -> Int
mostRight, mostLeft:: (Bounded a, Ord a) => Btree a -> a
mostLeft (NodeB list []) = head list
mostLeft (NodeB _ (x:_)) = mostLeft x
mostRight (NodeB list []) = last list
mostRight (NodeB _ xs) = mostRight $ last xs

levelBTree n= fst $ until condTree checkLevel (0,n)

condTree :: (Bounded a, Ord a) => (Int,Btree a)->Bool
condTree (_,NodeB _ list) = null list 

checkLevel :: (Bounded a, Ord a) => (Int,Btree a) -> (Int, Btree a)
checkLevel (i, NodeB _ ls) | null ls = (i,NodeB [] [])
 | otherwise = (i+1,head ls)

-- ������ 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree n tr@(NodeB a trees) | notCorHeight trees= False
                           | otherwise = (allKeysSorted tr) && (and $ map (\tr-> allKeysLong n tr) trees) && 
                             (length a)>=1 && (length a)<=(2*n-1)

notCorHeight :: (Bounded a, Ord a) => [Btree a] -> Bool 
notCorHeight [] = True
notCorHeight (tr:trs) = let h = (getHeight tr)
                        in (and $ map (\x->h==(getHeight x)) trs) 
                        && (and $ map (\x->notCorHeight x) trs) 
                        && (notCorHeight tr)

allKeysSorted :: (Bounded a, Ord a) => Btree a -> Bool 
allKeysSorted (NodeB a []) = isSorted a 
allKeysSorted (NodeB a trs) = isSorted a && (and $ map allKeysSorted trs) 
                              && and [allKeysLess (a!!y) (trs!!y) |y<-[0..(length a -1)]]
                              && and [allKeysMore (a!!y) (trs!!(y+1)) |y<-[0..(length a -1)]]

allKeysLess :: (Bounded a, Ord a) => a -> Btree a -> Bool 
allKeysLess _ (NodeB _ []) = True
allKeysLess v (NodeB keys _) = and $ map (\k->k<v) keys
allKeysMore :: (Bounded a, Ord a) => a -> Btree a -> Bool 
allKeysMore _ (NodeB _ []) = True
allKeysMore v (NodeB keys _) = and $ map (\k->k>v) keys

isSorted :: (Bounded a, Ord a) => [a] -> Bool 
isSorted (x:(y:_)) = x<y
isSorted _ = True

allKeysLong :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
allKeysLong n (NodeB keys []) = (length keys)>=(n-1) && (length keys)<=(2*n-1)
allKeysLong n (NodeB keys trees) = (length keys)>=(n-1) && (length keys)<=(2*n-1) 
                               && (and $ map (allKeysLong n) trees)

getHeight:: (Bounded a, Ord a)=>Btree a ->Int
getHeight tree = levelBTree tree

-- ������ 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ node1 node2 = (sort $ findAllElems node1) == (sort $ findAllElems node2)

findAllElems:: (Bounded a, Ord a ) => Btree a -> [a]
findAllElems (NodeB lst bs) = lst ++ concatMap findAllElems bs
-- ������ 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree node v = elem v (findAllElemsOrd node)  

findAllElemsOrd::  Ord a => Btree a -> [a]
findAllElemsOrd (NodeB lst bs) = lst ++ concatMap findAllElemsOrd bs

-- ������ 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t (NodeB el tree) v 
 | isFull t (NodeB el tree) = let (tr1,km, tr2) = splitAtB t (NodeB el tree) 
                              in treeSplit t (NodeB [km] [tr1,tr2]) v 
 | otherwise = treeSplit t (NodeB el tree) v 

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB lst _) = (2*t-1) == (length lst)  

insertKey :: Ord a => a -> [a] -> [a]
insertKey v (x:xv) | v < x = v:x:xv
 | otherwise = x:(insertKey v xv)
insertKey v [] = v:[]    

position :: Ord a => a -> [a] -> Int
position v xs= case findPos v (zip xs [0..]) of
                    Just x -> x
                    Nothing -> length xs

findPos :: Ord a => a -> [(a,Int)] -> Maybe Int
findPos v (z:zs) | v <=(fst z) = Just (snd z)
 | otherwise = findPos v zs 
findPos _ [] = Nothing                              

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB keys trees)= let km = keys !! (div t 2)
                                   (keys1, keys2) = splitAt (div t 2) keys
                                   (tr1, tr2) = splitAt ((+) (div t 2) 1) trees
                               in ((NodeB keys1 tr1), km, (NodeB (tail keys2) tr2)) 
      

treeSplit::  Ord a => Int-> Btree a->a-> Btree a
treeSplit t (NodeB el tree) v 
 | null tree = (NodeB (insertKey v el) tree) 
 | otherwise = let pos = (position v el) 
               in if isFull t (tree!!pos) 
  then let (tr1,km,tr2) = splitAtB t (tree!!pos) 
  in treeSplit t (NodeB (insertKey km el) ((take pos tree) ++ [tr1,tr2] ++ (drop (pos+1) tree))) v 
  else (NodeB el ((take pos tree) ++ [treeSplit t (tree!!pos) v] ++ (drop (pos+1) tree)))
------------------  -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX" [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" [], NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]