{-# OPTIONS_GHC -Wall #-}
module Zhulkevskyi03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix bs xs= bs == take (length bs) xs 

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (_,x,_) i [] 
 | i == 0 =x
 |otherwise = []
substitute tup@(subst,x,_) i w@(c:cs) 
 | i == 0 = x++ substitute tup (i-1) (drop (length subst) w)
 | otherwise = [c] ++ (substitute tup (i-1) cs)

-- Задача 3------------------------------------
splitToNStrings :: String -> Int->[String]
splitToNStrings w n 
 | length w <n = []
 | n == 0 = take (length w + 1) (repeat "")
 | otherwise = w: splitToNStrings (tail w) n
 
indexWords :: String -> Int-> [(String,Int)] 
indexWords w n=zip (splitToNStrings w n) [0..]

findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition s tup@(subst,_,_) = [ (tup,snd pair)|pair <-indexWords s (length subst),isPrefix subst (fst pair) ]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w= concatMap (\subst -> findPosition w subst) algo  

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (_,st,word)= (not isContinue,st+1,substitute subst i word)
 where (subst@(_,_,isContinue),i) = head (findAll algo word) 

-- Задача 6 ------------------------------------
calc :: Algorithm->ConfigA -> Int -> ConfigA
calc algo conf@(isContinue, steps, _) m 
 | isContinue && (steps<m) = calc algo (stepA algo conf) m 
 | otherwise = conf
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word 
 | finished == False = Just res 
 | otherwise = Nothing
    where (finished,_,res) = calc algo (True,0,word) m

-- Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg pr= maximum [maxInCommand command|command<-pr]
maxInCommand :: Command -> Int
maxInCommand (Z x) = x
maxInCommand (S x) = x
maxInCommand (T x y) = max x y
maxInCommand (J x y _) = max x y

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir= ir ++ (take (maximReg pr - length ir) (repeat 0))

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v= map (\(x,i) -> if i==r then v else x) (zip reg [0..])

-- Задача 9 ------------------------------------
doCommand :: ConfigC -> Command -> ConfigC
doCommand (nm,st,rg) (Z i) = (nm+1,st+1,upd rg (i-1) 0)
doCommand (nm,st,rg) (S i) = (nm+1,st+1,upd rg (i-1) ((rg !! (i-1))+1))
doCommand (nm,st,rg) (T i1 i2) = (nm+1,st+1, upd rg (i2-1) (rg !! (i1-1)))
doCommand (nm,st,rg) (J i1 i2 next) 
 | (rg !! (i1-1)) == (rg !! (i2-1)) = (next, st+1, rg) 
 | otherwise = (nm+1, st+1, rg)

stepC :: Program -> ConfigC -> ConfigC
stepC pr conf@(nm,_,_) = doCommand conf (pr !! (nm-1))

-- Задача 10 ------------------------------------
calcReg :: Program -> Int -> ConfigC -> ConfigC
calcReg pr mx conf@(nm,steps,_) 
 | (nm >(length pr)) || (steps>=mx) = conf 
 | otherwise = calcReg pr mx (stepC pr conf)
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir 
 | nm >(length pr) = Just x
 | otherwise = Nothing
  where (nm,_,(x:_)) = calcReg pr mx (1,0,ini pr ir) 
  --where (nm,_,(x:_)) = let conf@(num,st,rg) = (1,0,ini pr ir)
--in until ((num <=(length pr))&&(st<mx)) (stepC $ pr) conf

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
