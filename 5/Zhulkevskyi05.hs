{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne st c | elem c st = sort st
 | otherwise = sort (st++[c])

addAll :: String -> String -> String 
addAll st wd = foldl addOne st wd

addWithout :: String -> String -> String 
addWithout st wd = addAll st $ filter (\a->a/='$') wd

inter :: String -> String -> String 
inter st1 st2 = sort $ filter (\a->elem a st2) st1 

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict pt n = if null res then "" else (snd (head res)) 
 where res = filter (\pr->(fst pr) == n) pt

upPredict :: Predict -> Char -> String -> Predict 
upPredict [] n st= [(n,st)]
upPredict (x:pl) n st | (fst x) == n = (n,st):pl 
 | (fst x)>n= ((n,st):[x])++pl 
 | otherwise= x:(upPredict pl n st) 
 

-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl word = case step gr ctl (word++['$'],(findZeroNotTerm ctl):['$'],Just []) of
 (_,_,res) -> res

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl ((x:xs),(y:ys), Just result) |isUpper y= case findResult ctl y x of 
  Just res -> step gr ctl (x:xs,(snd $ gr !! res)++ys,Just (result++[res]))
  Nothing -> ([],[],Nothing)
 | y==x = step gr ctl (xs,ys,Just result)
 | otherwise = ([],[],Nothing)
step _ _ ([],[],result) = ([],[], result)

step _ _ _ = ([],[],Nothing)

findResult :: Control -> Char->Char -> Maybe Int
findResult [] _ _ = Nothing
findResult (x:rest) st inp | (st ==(fst $ fst x)) && ((snd $ fst x) ==inp) =Just (snd x)
 | otherwise = findResult rest st inp
findZeroNotTerm :: Control -> Char
findZeroNotTerm (x:xs) | snd x == 0 = fst $ fst x
 | otherwise=findZeroNotTerm xs
findZeroNotTerm [] = ' '
-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ [] = ['$']
first pFst (x:xs) | isUpper x = let res = tkPredict pFst x 
  in if elem '$' res then addWithout (first pFst xs) res else res 
 | x=='$' = first pFst xs
 | otherwise = [x]

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl gr pFst pNxt = sort $buildCtr (zip gr [0..]) pFst pNxt
buildCtr::[((Char,String),Int)] ->Predict -> Predict->  Control
buildCtr [] _ _ = []
buildCtr (((x,pr),i):rest) pFst pNxt= let res=first pFst pr
                                          extRes = tkPredict pNxt x
 in if elem '$' res then (makeControl x (addWithout extRes res) i)++buildCtr rest pFst pNxt else (makeControl x res i)++buildCtr rest pFst pNxt
makeControl:: Char ->String-> Int -> Control 
makeControl nt wd i= map (\t -> ((nt,t),i)) wd 
-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = isUnique $ map (fst) (buildingControl gr pFst pNxt)

isUnique ::[(Char,Char)]-> Bool
isUnique [] = True 
isUnique (x:xs) |null $ filter ((==)x) xs= isUnique xs 
 |otherwise = False

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
buildFst gr =sort $ map (\ch-> (ch,makeFirsts gr ch)) (findNTs gr)

makeFirsts :: Grammar -> Char -> String 
makeFirsts gr ch = sort$ nub $ concatMap (\str->if null str then ['$'] else 
                                               if isUpper (head str) then 
                                               if ch == (head str) then [] else addWithout [] (makeFirsts gr (head str)) else [head str]) 
                                               (findProductions gr ch)  

findNTs :: Grammar -> [Char]
findNTs gr = nub $ map (fst) gr

findProductions :: Grammar -> Char -> [String]
findProductions [] _ = []
findProductions (x:xs) ch | fst x ==ch = (snd x): findProductions xs ch
 | otherwise = findProductions xs ch
 
-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt = undefined
--buildNxt gr pFst = let nts = findNTs gr
--                       init = [(head nts,"$")]
--                       initN = evalNxt (nontermTails gr) pFst init
 --in until (evalNxt (nontermTails gr) pFst) init
-- in fst$ until (\(n,n1) -> n == n1) (\(n,n1) -> (n1, evalNxt (nontermTails gr) pFst n1)) (init, initN)
--nontermTails :: Grammar -> [(Char,String)] 
--nontermTails gr = concatMap (\(ch,str) -> makeTails ch str) gr 

--makeTails :: Char -> String -> [(Char,String)]
--makeTails _ [] = []
--makeTails ch (x:xs) =if not (isUpper x) then (ch,xs):makeTails ch xs else makeTails ch xs  

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
--evalNxt tails pFst pNxt= upPredict if 
evalNxt =undefined
extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne =undefined
--extandNxtOne pFst n pNxt (m:st)= 

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]
