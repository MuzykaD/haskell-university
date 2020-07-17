{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi10 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Seq x y) = Seq (simplify x) (simplify y)
simplify (Alt x y) = Alt (simplify x) (simplify y)
simplify (Rep x) = Rep (simplify x)
simplify (Plus x) = Seq (simplify x) (Rep (simplify x))
simplify (Opt x) = Alt (simplify x) (Null)
simplify r = r


-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_,fs,_) i= elem i fs

isEssential :: Automation -> State -> Bool 
isEssential (_,fs,trs) i= (elem i fs) 
 || (not $ null$ filter (\(st1,_,lbl)->(st1==i)&&(case lbl of {C _ -> True; _-> False})) trs)

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_,_,trs) i= filter (\(st1,_,_)->i==st1) trs

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trs = nub $ filter (\lb->case lb of {C _ ->True; _ -> False}) (map (\(_,_,c)-> c) trs)

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA aut@(bs,_,_) s= let (finSt,finS,bool) = until cond1 (acceptDStep aut) (bs,s,False)
 in (isTerminal aut finSt) && (null finS) && (not bool)

cond1::(State,String,Bool) -> Bool
cond1 (_,s,bool)= bool || (null s)

acceptDStep :: Automation ->(State,String,Bool) -> (State,String,Bool)
acceptDStep aut (bs,s,_)= let curr = head s
                              availTr = transitionsFrom aut bs
                              nextSt = findStateForLabel availTr curr
 in case nextSt of
    Just st -> (st,tail s, False)
    Nothing -> (bs,s,True)

findStateForLabel :: [Transition] -> Char -> Maybe State 
findStateForLabel ((_,st,(C c)):ts) curr | curr == c = Just st
 | otherwise = findStateForLabel ts curr
findStateForLabel _ _ = Nothing

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep aut st lbl = let trs = (transitionsFrom aut st)
 in map (\(_,st2,_)-> st2) (filter (\(_,_,lbl1) -> lbl1==lbl) trs)

setStep aut sts lbl= concatMap (\st1 -> stStep aut st1 lbl) sts 

closure aut ss =snd $ until (\(ss1,_)->null ss1) (closStep aut) (ss,[])

closStep :: Automation -> ([State],[State]) -> ([State],[State])
closStep aut (ss1,ss2) = let sts = setStep aut ss1 Eps 
                             sts1 = filter (\st -> notElem st ss2) sts
 in (sts1,sort (ss2++sts1))

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut@(bs,_,_) s | isDet aut = acceptsDA aut s
 | otherwise = let (finSt,finS) = until condNDA (ndaStep aut) ([bs],s)
   in (null finS) && any (\fs -> isTerminal aut fs) (nub $ finSt++(closure aut finSt))

condNDA::([State],String)->Bool
condNDA (sts,ss) = (null sts) || (null ss)


ndaStep :: Automation  -> ([State],String) -> ([State],String)
ndaStep aut (bs,s) = let curr = head s
                         statesFrom = closure aut bs
                         availTr = setStep aut (nub(bs++statesFrom)) (C curr)
 in (availTr,tail s)

isDet:: Automation-> Bool
isDet au@(_,_,trans) = (null $ filter (\(_,_,lbl)-> lbl == Eps) trans) 
 && null (filter (\(st1,_,_) -> let trl = transitionsFrom au st1
  in ((length $ labels trl) /= (length $ map (\(_,_,lb)->lb) trl))) trans)

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null st fin next= ([(st,fin,Eps)],next)
make (Term c) st fin next = ([(st,fin,C c)],next)
make (Seq r1 r2) st fin next = let (tr1,nextST1) = make r1 st next (next+2)
                                   (tr2,nextST2) = make r2 (next+1) fin nextST1
 in (tr1++tr2++[(next,next+1,Eps)],nextST2)
make (Alt r1 r2) st fin next = let (trs1,nextST1) = make r1 next (next+1) (next+4)
                                   (trs2,nextST2) = make r2 (next+2) (next+3) nextST1
                                   tr3 = (st,next,Eps)
                                   tr4 = (st,next+2,Eps)
                                   tr5 = (next+1,fin,Eps)
                                   tr6 = (next+3,fin,Eps)
                                   trs = tr3:tr4:tr5:tr6:[]
 in (trs1++trs2++trs,nextST2)
make (Rep r1) st fin next = let (trs1,nextST1) = make r1 next (next+1) (next+2)
                                tr2 = (st,fin,Eps)
                                tr3 = (st,next,Eps)
                                tr4 = (next+1,fin,Eps)
                                tr5 = (next+1,next,Eps)
                                trs = tr2:tr3:tr4:tr5:[]
 in (trs1++trs,nextST1)
make (Plus _) _ _ _ = error "Plus is ignored because re is simplified!"
make (Opt _) _  _ _ = error "Opt is ignored because re is simplified!"
-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg s= case (P.parse reg "" s) of
      Right re -> Just re
      Left _ -> Nothing


rsymb :: P.Parser RE
rsymb = do c <- P.noneOf "()|*?+"
           return (Term c)

prime :: P.Parser RE
prime = do re <- (rsymb P.<|> parensExp)
           return re

rfact :: P.Parser RE
rfact = do re <- prime 
           cs <- P.many (P.oneOf "*?+")
           return (buildSign cs re)

rterm :: P.Parser RE
rterm = do rf <- rfact
           rfs <- P.many rfact
           return (case rfs of
               [] -> rf
               rfs1 -> (Seq rf (buildSeq rfs1)))
orterm :: P.Parser RE
orterm = do _ <- P.char '|'
            rt <- rterm
            return rt
rexpr :: P.Parser RE
rexpr = do rt<- rterm
           rts <- P.many orterm
           return (case rts of
               [] -> rt
               rts1 -> (Alt rt (buildAlt rts1)))

parensExp :: P.Parser RE
parensExp = do _ <- P.char '('
               re <- rexpr
               _ <- P.char ')'
               return re

reg :: P.Parser RE
reg = do re <- rexpr
         P.eof
         return (re)


buildSeq :: [RE] -> RE
buildSeq [x] = x
buildSeq (x:xs)= Seq x (buildSeq xs)
buildSeq [] = error "Empty list"

buildAlt :: [RE] -> RE
buildAlt [x] = x
buildAlt (x:xs)= Alt x (buildAlt xs)
buildAlt [] = error "Empty list"

buildSign :: String ->RE -> RE
buildSign (x:xs) re = case x of
     '*' -> buildSign xs (Rep re)
     '?' -> buildSign xs (Opt re)
     '+' -> buildSign xs (Plus re)
     _ -> Null
buildSign [] re = re
-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(bs,_,_) = let (gmsx,bmsx,mlx)= until condMeta (stepMeta aut) (init1 aut bs)
 in (head gmsx,bmsx, nub mlx)

init1 :: Automation -> State -> ([MetaState], [MetaState], [MetaTransition])
init1 aut bs = ([],[filter (isEssential aut) ([bs]++closure aut [bs])],[]) 

condMeta:: ([MetaState], [MetaState], [MetaTransition]) ->Bool
condMeta (_,bmsx,_)=null bmsx
stepMeta :: Automation -> ([MetaState], [MetaState], [MetaTransition])->([MetaState], [MetaState], [MetaTransition])
stepMeta aut (gmsx,bmsx,mtrx) = let curr = head bmsx 
                                    transFrom = concatMap (transitionsFrom aut) curr
                                    els1 = labels transFrom
                                    zi = zip els1 [0..]
                                    metaSts = map (\el-> let t =setStep aut curr el
                                      in filter (isEssential aut) (nub(t++(closure aut t)))) els1
                                    mtxs = map (\(el,i)-> (curr,metaSts!!i,el)) zi
                                    newMeta = filter (\m->notElem m bmsx) metaSts
                                    newMeta2 = filter (\m->notElem m gmsx) newMeta
  in ((gmsx++[curr]),(tail bmsx)++newMeta2,mtrx++ mtxs)
makeDA :: Automation -> Automation
makeDA aut = let (gmsx,_,mlx) = makeDA' aut
                 (_,ls,trs) = foldl (findAnswer aut) ([(gmsx,1)],[],[]) mlx
                 --sts = map (\(m1,m2,c)->(findPaired zipped m1, findPaired zipped m2,c))mlx
   in (1,sort $ nub ls, sort trs)

findPaired :: [(MetaState,Int)]->MetaState-> Maybe Int
findPaired (x:xs) m | fst x == m = Just (snd x)
 | otherwise = findPaired xs m
findPaired [] _ = Nothing
findAnswer :: Automation ->([(MetaState,Int)],[Int],[Transition])-> MetaTransition -> ([(MetaState,Int)],[Int],[Transition])

findAnswer aut@(_,fs,_) (zi,ls,trs)  (ms1,ms2,c) = let nmst1 = case findPaired zi ms1 of
                                                                   Just i -> i
                                                                   Nothing -> (snd (last zi))+1
                                                       zi1 = nub (zi ++ [(ms1,nmst1)])
                                                       nmst2 = case findPaired zi1 ms2 of
                                                                   Just i -> i
                                                                   Nothing -> (snd (last zi))+1
                                                       zi2 = (zi1 ++ [(ms2,nmst2)])
                                                       zf = nub zi2
                                                      --trs = concatMap (transitionsFrom aut) ms2
                                                      --st2s = map (\(_,s,_)->s) trs
 in if any (\st ->elem st fs) (nub $ ms2++(closure aut ms2)) 
     then (zf,ls++[nmst2],trs++[(nmst1,nmst2,c)])
     else (zf,ls,trs++[(nmst1,nmst2,c)])
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )