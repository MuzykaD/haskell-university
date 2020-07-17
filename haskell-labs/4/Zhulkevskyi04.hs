{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi04 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG::String -> Bool
analyseG st1 = case s st1 of
 Just st2 -> null st2
 Nothing -> False

s :: String -> Maybe String
s ('a':st1)= case s st1 of 
 Just ('b':st2)  -> case a st2 of
  Just ('a': st3) -> Just st3
  _ -> Nothing 
 _ -> Nothing
s ('b':st1) = Just st1
s _ = Nothing

a :: String -> Maybe String
a ('a':st1)= Just st1
a ('b':'a':st1) = case a st1 of 
 Just st2 -> s st2
 Nothing -> Nothing

a _ = Nothing

-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance st= case b st of
 Just st1 -> null st1
 Nothing -> False  

b :: String -> Maybe String 
b ('{':st) = case b st of 
 Just ('}':st1) -> b st1
 _ -> Nothing 
b ('(':st) = case b st of 
 Just (')':st1) -> b st1
 _ -> Nothing 
b ('[':st) = case b st of 
 Just (']':st1) -> b st1
 _ -> Nothing 
b (' ':st) = b st
b st = Just st

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr st1 = case ae st1 of 
   Just st2 -> null st2  
   _          -> False 

ae :: String -> Maybe String 
ae st1= case af st1 of
 Just st2 -> aa st2
 Nothing -> Nothing

aa :: String -> Maybe String 
aa (c:st1) 
 | elem c "*-+" = case af st1 of
  Just st2 -> aa st2
  Nothing -> Nothing
aa st1 = Just st1
 
af :: String -> Maybe String 
af ('(':st1) = case ae st1 of 
     Just (')':st2) -> Just st2 
     _                -> Nothing
     
af (d:st1) | isDigit d = Just st1
af _ = Nothing  

-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of 
   Just (v,st2)| null st2 -> Just v 
   _ -> Nothing

le :: String -> Maybe (Int,String) 
le st1 = case lf st1 of 
     Just (v1,st2) -> la (v1,st2) 
     Nothing       -> Nothing 

la :: (Int,String) -> Maybe (Int,String) 
la (v1,(d:st1))| elem d "-+*" = case lf st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else if d=='*' then v1*v2 else v1-v2 
                      in la (v,st2)  
     Nothing -> Nothing 
la (v1,st1) = Just (v1,st1)

lf :: String -> Maybe (Int,String) 
lf ('(':st1) = case le st1 of 
     Just (v,(')':st2)) -> Just (v,st2) 
     _ -> Nothing  

lf (d:st1) | isDigit d = Just (digitToInt d,st1) 
lf _ = Nothing


-- Задача 5 -----------------------------------------
evalRigth :: String -> Maybe Int 
evalRigth st1= case re st1 of 
   Just (v,st2)| null st2 -> Just v 
   _ -> Nothing

re :: String -> Maybe (Int,String) 
re st= case rf st of
 Just (v,st1) -> ra (v,st1)
 Nothing -> Nothing 

ra :: (Int,String) -> Maybe (Int,String) 
ra (v,(c:st)) | elem c "*-+" = case re st of
 Just (v1,st1) -> let res = if c=='*' then v*v1 else if c=='+' then v+v1 else v-v1
  in Just (res,st1)
 Nothing -> Nothing
ra (v,st) = Just (v,st)

rf :: String ->  Maybe (Int,String) 
rf ('(':st) = case re st of 
 Just (v,(')':st1)) -> Just (v,st1)
 _ -> Nothing
rf (d:st) | isDigit d = Just (digitToInt d, st)
rf _ = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior st1= case pe st1 of 
   Just (v,st2)| null st2 -> Just v 
   _ -> Nothing

pe :: String -> Maybe (Int,String) 
pe st = case pt st of 
 Just (v,st1) -> pa (v,st1) 
 Nothing -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (v1,(d:st1)) | elem d "-+" = case pt st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else v1-v2 
                      in pa (v,st2)  
     Nothing -> Nothing 
pa (v1,st1) = Just (v1,st1)

pt :: String -> Maybe (Int,String) 
pt st= case pf st of
 Just (v,st1) -> pb (v,st1)
 Nothing -> Nothing 
 

pb :: (Int,String) -> Maybe (Int,String) 
pb (v,('*':st)) = case pf st of
  Just (v1,st1) -> pa (v*v1,st1)
  Nothing -> Nothing
pb (v,st) = Just (v,st)   

pf :: String -> Maybe (Int,String) 
pf (d:st) | isDigit d = Just (digitToInt d,st) 
pf ('(':st) = case pe st of
 Just (v,(')':st1)) -> Just (v,st1)
 _ -> Nothing
pf _ = Nothing
