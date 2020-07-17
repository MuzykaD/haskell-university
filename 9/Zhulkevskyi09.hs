{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi09 where

import Data.List
-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b [] = [(a,b)]
updateValue a b (x:xs) | a == fst x = (a,b):xs
 | otherwise = x:(updateValue a b xs)


-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A xs) (I i1) (I i2) = A (updateValue i1 i2 xs)
updateArray xs _ _ = xs

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp (Add) (I x) (I y)= I (x+y)
applyOp (Minus) (I x) (I y)= I (x-y)
applyOp (Mul) (I x) (I y) = I (x*y)
applyOp (Less) (I x) (I y) | x<y = I 1
 | otherwise = I 0
applyOp (Equal) (I x) (I y) | x==y = I 1
 | otherwise = I 0
applyOp (Index) (A xs) (I i) = case lookup i xs of
       Just x -> I x
       Nothing -> I 0
applyOp _ _ _ = error "not matched"


-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const x) _ _=I x
evExp (Var y) _ st = findValueInState st y
evExp (OpApp op arg1 arg2) dfx st = applyOp op (evExp arg1 dfx st) (evExp arg2 dfx st)
evExp (Cond con arg1 arg2) dfx st = case evExp con dfx st of
       I 0 -> evExp arg2 dfx st
       _ -> evExp arg1 dfx st
evExp (FunApp fn args) dfx st = let (vars,f)=findFuncValue dfx fn
                                    stringVars = map (\x ->case x of 
                                                           Arr d -> d 
                                                           Int y -> y
                                                     ) vars
                                    newSt = zip stringVars (evArgs args dfx st)
 in evExp f dfx newSt
     
findFuncValue :: [FunDef]-> String -> ([VarDef],Exp)
findFuncValue ((name,val):xs) y | name == y = val
 | otherwise = findFuncValue xs y 
findFuncValue [] _ = error "Function Is Missing"

findValueInState :: StateP -> String -> Value
findValueInState (x:xs) y | fst x == y = snd x 
 | otherwise = findValueInState xs y 
findValueInState [] _ = error "Value Is Missing"

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs exps dfx st = map (\expret ->evExp expret dfx st) exps 


-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign s exp1) fd _ st = insertValInSt st (evExp exp1 fd st) s
evStmt (AssignA s exp1 exp2) fd _ st = let  oldAr = findValueInState st s
                                            evExp1 = evExp exp1 fd st
                                            evExp2 = evExp exp2 fd st
                                            newAr= updateArray oldAr evExp1 evExp2
  in insertValInSt st newAr s 
evStmt (If exp1 stmt1 stmt2) fd pd st = case evExp exp1 fd st of
  I 0 -> evStmt stmt2 fd pd st
  _ -> evStmt stmt1 fd pd st
evStmt (Call s expres) fd pd st = let (varD,stmt1) = findProc pd s
                                      values = map (\e-> evExp e fd st) expres
                                      stringVars = map (\x ->case x of 
                                          Arr d -> d 
                                          Int y -> y) varD
                                      tempSt = zip stringVars values
                                      lglSt = tempSt++st
                                      finSt = evStmt stmt1 fd pd lglSt 
 in foldl eraseLocalVar finSt stringVars

evStmt (While exp1 stmt) fd pd st = until (cond1 exp1 fd) (evStmt stmt fd pd) st

evStmt (Block varD stmts) fd pd st = let inSt = foldl (\st1 v -> [initv v]++st1) st varD
                                         stringVars = map (\x ->case x of 
                                                 Arr d -> d 
                                                 Int y -> y) varD
                                         finSt =foldl (\st2 stm->evStmt stm fd pd st2) inSt stmts 
 in foldl (eraseLocalVar) finSt stringVars

cond1 :: Exp -> [FunDef] -> StateP -> Bool
cond1 exp1 fd st = case evExp exp1 fd st of 
   I 0 -> True
   _ ->False

findProc :: [ProcDef] -> Id -> ([VarDef],Stmt)
findProc (x:xs) s | fst x == s = snd x
 | otherwise = findProc xs s
findProc [] _ = error "Procedure Is Missing"
insertValInSt :: StateP->Value -> Id ->StateP
insertValInSt (x:xs) val s | s == (fst x) = (s,val):xs
 |otherwise = x:(insertValInSt xs val s)
insertValInSt [] val s = [(s,val)]

eraseLocalVar :: StateP -> Id -> StateP
eraseLocalVar (x:xs) s | fst x == s = xs
 | otherwise = x:(eraseLocalVar xs s)
eraseLocalVar [] s = error s
-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Const _) _ _= Just It
iswfExp (Var s) vEnv _ = findValType vEnv s
iswfExp (OpApp op arg1 arg2) vEnv fEnv = case (iswfExp arg1 vEnv fEnv,iswfExp arg2 vEnv fEnv) of
 (Just t1,Just t2) -> iswfOp op [t1,t2]
 _ -> Nothing
iswfExp (Cond c a1 a2) vEn fEn = case (iswfExp c vEn fEn,iswfExp a1 vEn fEn,iswfExp a2 vEn fEn) of
 (Just t1,Just t2, Just t3) -> iswfCond [t1,t2,t3]
 _ -> Nothing
iswfExp (FunApp fn args) vEnv fEnv = case findFuncType fEnv fn of
 Just ts -> let mayTypes = (map (\a -> iswfExp a vEnv fEnv) args) 
        in if (isValidList mayTypes)
           then if ts == (map (\x -> case x of {Just typ -> typ;Nothing->It;}) mayTypes) 
                then Just It 
                else Nothing
           else Nothing 
 Nothing -> Nothing


findFuncType :: FunEnv -> Id -> Maybe [Type]
findFuncType (x:xs) s | fst x == s = Just (snd x)
 | otherwise = findFuncType xs s
findFuncType [] _ = Nothing
findValType :: VarEnv -> Id -> Maybe Type
findValType (x:xs) s | fst x == s = Just (snd x)
 | otherwise = findValType xs s
findValType [] _ = Nothing
isValidList :: [Maybe Type] -> Bool
isValidList ((Just _):xs) = isValidList xs 
isValidList ((Nothing):_) = False
isValidList [] = True

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign s exp1) ve fe pe = (checkAllEnv ve fe pe) 
  && (case findValType ve s of
       Just t -> case iswfExp exp1 ve fe of 
              Just t1->t1 == t
              Nothing -> False
       Nothing -> False)
iswfStmt (AssignA s exp1 exp2) ve fe pe = (checkAllEnv ve fe pe)
 && (case (findValType ve s, iswfExp exp1 ve fe,iswfExp exp2 ve fe) of
       (Just t1, Just t2, Just t3) -> iswfAssignA [t1,t2,t3]
       _ -> False)
iswfStmt (If exp1 stmt1 stmt2) ve fe pe = (checkAllEnv ve fe pe)
 && (case iswfExp exp1 ve fe of
       Just It-> True
       _ -> False) && (iswfStmt stmt1 ve fe pe) && (iswfStmt stmt2 ve fe pe) 
iswfStmt (Call s expres) ve fe pe= (checkAllEnv ve fe pe)
 && (case findProcType pe s of
       Just ts -> let mayTypes = map (\e->iswfExp e ve fe) expres
           in if (isValidList mayTypes)
              then ts == (map (\x -> case x of {Just typ -> typ;Nothing->It;}) mayTypes)
              else False 
       Nothing -> False)
iswfStmt (While exp1 stmt) ve fe pe = (checkAllEnv ve fe pe)
 && (case iswfExp exp1 ve fe of
       Just It -> True
       _ -> False) && (iswfStmt stmt ve fe pe)
iswfStmt (Block varD stmts) ve fe pe = let newVE = [case v of 
                                                         Arr s -> (s,At)
                                                         Int s-> (s,It) |v<-varD]
 in (checkVEnv newVE)
 && (checkAllEnv ve fe pe) 
 && (and $ map (\st->iswfStmt st (newVE++ve) fe pe) stmts)
 

findProcType :: ProcEnv -> Id -> Maybe [Type]
findProcType (x:xs) s | fst x == s = Just (snd x)
 | otherwise = findProcType xs s
findProcType [] _ = Nothing
checkAllEnv :: VarEnv -> FunEnv -> ProcEnv -> Bool
checkAllEnv ve fe pe = (checkVEnv ve) && (checkFEnv fe) &&(checkPEnv pe)
checkVEnv :: VarEnv -> Bool
checkVEnv ve = length ve ==(length $ nub $ map (\v-> fst v) ve)
checkFEnv :: FunEnv -> Bool
checkFEnv fe = length fe ==(length $ nub $ map (\f-> fst f) fe)
checkPEnv :: ProcEnv -> Bool
checkPEnv pe = length pe ==(length $ nub $ map (\p-> fst p) pe)
-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (id1,(vds,exp1)) fe= let ve = [case v of 
                                           Arr s -> (s,At)
                                           Int s-> (s,It) |v<-vds] 
 in case findFuncType fe id1 of
       Just _-> case iswfExp exp1 ve fe of
              Just It -> True
              _->False 
       Nothing -> False
-- Is it correct not to have proc in procEnv?? 
-- Всі процедури і функції, що використовуються – визначені, 
-- кожний раз застосовуються до вірної кількості аргументів, кожний з яких має відповідний тип.
iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_,(vds,stm1)) ve fe pe=  let vel = [case v of 
                                                    Arr s -> (s,At)
                                                    Int s-> (s,It) |v<-vds] 
 in iswfStmt stm1 (vel++ve) fe pe
              

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (vd,fd,pd)= let ve = [case v of 
                                   Arr s -> (s,At)
                                   Int s-> (s,It) |v<-vd] 
                            fe = makeFEnvFromDef fd 
                            pe = makePEnvFromDef pd
 in (checkAllEnv ve fe pe) 
    && (checkMain pd)
    && (and $(map (\f -> iswfFunDef f fe) fd))
    && (and $(map (\p->iswfProcDef p ve fe pe) pd))

makeFEnvFromDef::[FunDef] -> FunEnv
makeFEnvFromDef ((id1,(vds,_)):xs) = let varT = [case v of 
                                                  Arr _ -> At
                                                  Int _-> It |v<-vds]
 in (id1,varT):makeFEnvFromDef xs
makeFEnvFromDef [] = []

makePEnvFromDef::[ProcDef] -> ProcEnv
makePEnvFromDef ((id1,(vds,_)):xs) = let varT = [case v of 
                                                  Arr _ -> At
                                                  Int _-> It |v<-vds]
 in (id1,varT):makePEnvFromDef xs
makePEnvFromDef [] = []

checkMain :: [ProcDef] -> Bool
checkMain ((id1,(vds,_)):xs) | (id1 == "main") && (vds == []) = True
 | otherwise = checkMain xs
checkMain [] = False

--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
