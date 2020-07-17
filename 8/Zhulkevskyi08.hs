{-# OPTIONS_GHC -Wall #-}
module ZhulkevskyiP08 where
import Data.List
import Text.ParserCombinators.Parsec

data Recur = Zero | Succ | Sel Int Int 
    | Super Recur [Recur] 
    | Prim Recur Recur 
    | Mini Recur Int 
    | Name String  deriving (Show, Eq)
type System = [(String,Recur)] 


-- ������ 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool 
isNumbConst _ (Zero)= True 
isNumbConst syst (Super Succ i) = isNumbConst syst (head i)
isNumbConst syst (Super Zero i) = isNumbConst syst (head i)
isNumbConst syst (Name f) = case (findRecur syst f) of 
  Just recu -> isNumbConst syst recu
  Nothing -> False
isNumbConst _ _ = False

-- ������ 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank _ (Zero) = 1
evRank _ (Succ) = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = evRank syst (st)- 1 
evRank syst (Mini b _) = evRank syst b-1
evRank syst (Name f) = case (findRecur syst f) of 
    Just recu -> evRank syst recu
    Nothing -> 0

findRecur :: System -> String -> Maybe Recur
findRecur (x:xs) str | fst x == str = Just (snd x)
 | otherwise = findRecur xs str 
findRecur [] _ = Nothing


-- ������ 3 ------------------------------------
isNames :: System -> Bool 
isNames syst = let names= fst $ unzip syst 
 in ((length $ nub names) == (length names)) && (and $ map (checkCorrectFunctionName names) syst)

checkCorrectFunctionName :: [String] ->(String, Recur) -> Bool
checkCorrectFunctionName names (str,recu) = let namesRec = nub $ findNameRecur recu [] 
 in if null namesRec then True else and $ map (compareSequence (zip names [0..]) str) namesRec 

compareSequence :: [(String, Int)]->String -> String -> Bool
compareSequence names str s= case (findNumb names s) of
  Just i -> case (findNumb names str) of
    Just j -> j>i
    Nothing -> False
  Nothing -> False  

findNumb :: [(String,Int)] -> String -> Maybe Int
findNumb (x:xs) str | fst x == str = Just (snd x)
   | otherwise = findNumb xs str 
findNumb [] _ = Nothing

findNameRecur :: Recur -> [String]-> [String]
findNameRecur (Prim a b) xs = (findNameRecur a xs) ++ (findNameRecur b xs)
findNameRecur (Mini b _) xs = (findNameRecur b xs)
findNameRecur (Super a bs) xs = (findNameRecur a xs) ++ (concatMap (\r->findNameRecur r xs) bs)
findNameRecur (Name f) xs = f:xs
findNameRecur _ _ = []
-- ������ 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ (Zero) = True
isRecur _ (Succ) = True
isRecur _ (Sel i j) = (i>=1) && (j>=1) && (j<=i)
isRecur syst (Super f al) = let r = evRank syst f
 in (r == length al) && (and $ map (isRecur syst) al) && (isRecur syst f)
    && (allEqual $ map (evRank syst) al)
isRecur syst (Prim i st) 
 | isNumbConst syst i = (isRecur syst i) && (isRecur syst st) 
                        && ((evRank syst st)==2) && ((evRank syst i)==1)
 | otherwise = (isRecur syst i) && (isRecur syst st) 
                && (((evRank syst st) - (evRank syst i))==2)
isRecur syst (Mini b _) = (isRecur syst b) && ((evRank syst b) >1)
isRecur syst (Name f) = case (findRecur syst f) of 
  Just res -> isRecur syst res
  Nothing -> False

allEqual :: [Int] -> Bool 
allEqual xs = and $ map (== head xs) (tail xs)
-- ������ 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval _ (Zero) _ = 0 
eval _ (Succ) xs= head xs + 1
eval _ (Sel _ x) xs = xs !! (x-1)
eval syst (Super f al) xs = eval syst f (map (\recu-> eval syst recu xs) al) 
eval syst (Name str) xs = case findRecur syst str of 
  Just recu -> eval syst recu xs  
  Nothing -> 0
eval syst (Prim r1 r2) xs =last $ fst $ until cond1 (stepEval syst r2) (init xs++[0]++ [eval syst r1 (take (evRank syst r1) xs)],last xs)
eval syst minRec@(Mini _ _) xs= case evalPart syst minRec xs of 
    Just res -> res
    Nothing -> 0
cond1::([Int],Int)->Bool
cond1 (xs,i)= i <= (xs !! (length xs -2))

stepEval::System -> Recur -> ([Int],Int) -> ([Int],Int)
stepEval syst r1 (xs,i)= let without1 = init xs
                             counter = (last without1)
                             without2 = init without1
                         in (without2 ++ [counter+1]++[eval syst r1 (without2++[counter]++[last xs])],i)
-- ������ 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart _ (Zero) _ = Just 0 
evalPart _ (Succ) xs= Just (head xs + 1)
evalPart _ (Sel _ x) xs = Just (xs !! (x-1))
evalPart syst (Super f al) xs = case evaluateListRec syst al xs [] of
    Just res -> evalPart syst f res
    Nothing -> Nothing                           
evalPart syst (Name str) xs = case findRecur syst str of 
  Just recu -> evalPart syst recu xs  
  Nothing -> Nothing
evalPart syst (Prim r1 r2) xs = case evalPart syst r1 (take (evRank syst r1) xs) of 
    Just initial -> case fst (until cond2 (stepEvalPart syst r2) (Just (init xs++[0]++[initial]),last xs)) of
        Just zs -> Just (last zs)
        Nothing -> Nothing
    Nothing -> Nothing

evalPart syst (Mini b i) xs = let (ys,_,res)= until cond3 (minStep syst b) (Just (xs++[-1]),i,1)
 in case ys of 
    Just zs -> if (res==0) then Just (last zs) else Nothing 
    Nothing -> Nothing 

cond3::(Maybe [Int],Int,Int) -> Bool
cond3 (xs,i,resPred) = case xs of
    Just ys -> (resPred <=0)||((last ys) >=i)
    Nothing -> False

minStep :: System -> Recur -> (Maybe [Int],Int,Int)-> (Maybe [Int],Int,Int)
minStep syst r (ys,i,predRes) = case ys of
    Just xs -> case evalPart syst r (init xs++[last xs+1]) of
        Just res-> (Just (init xs++[last xs+1]),i,res) 
        Nothing -> (Nothing,i,predRes)
    Nothing -> (Nothing,i,predRes)

cond2::(Maybe [Int],Int)->Bool
cond2 (xs,i)= case xs of 
    Just ys -> i <= (ys !! (length ys -2))
    Nothing -> True

stepEvalPart::System -> Recur -> (Maybe [Int],Int) -> (Maybe [Int],Int)
stepEvalPart syst r1 (ys,i)= case ys of
    Just xs -> let without1 = init xs
                   counter = (last without1)
                   without2 = init without1
               in case evalPart syst r1 xs of
                       Just res -> (Just (without2 ++ [counter+1]++[res]),i)
                       Nothing -> (Nothing,i)
    Nothing -> (Nothing, i)
    

evaluateListRec:: System -> [Recur]-> [Int] -> [Int]-> Maybe [Int]
evaluateListRec syst (y:rest) xs res= case evalPart syst y xs of
     Just i -> evaluateListRec syst rest xs (res++[i])
     Nothing -> Nothing
evaluateListRec _ [] _ res = Just res
-- ������ 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec input = case parse system "" (filter (\c ->(c/='\n') && (c/='\\') && (c/=' ')) input) of
    Right syst -> Just syst
    Left _ -> Nothing
integer :: Parser Int
integer = do st <-many1 digit
             return (read st)  

name1 :: Parser Recur
name1 = do c <- letter
           st <- many (letter <|> digit) 
           return (Name (c:st))
iden :: Parser String
iden = do c <- letter
          st <- many (letter <|> digit) 
          return (c:st)

recur :: Parser Recur
recur = do r <- try base <|> try super <|> try prim <|> mini
           return r
base :: Parser Recur
base = do r <- try zero <|> try succ1 <|> try sel <|> name1
          return r

succ1 :: Parser Recur
succ1 = do _ <- string "a1"
           return (Succ)    
zero :: Parser Recur
zero = do _ <- string "z1"
          return (Zero)
sel :: Parser Recur
sel = do _<-char 's'
         i1 <- digit
         i2 <- digit
         return (Sel (read [i1]) (read [i2]))
          
super :: Parser Recur
super = do _<- char '('
           r1 <- recur
           _<- char ':'
           r2 <- recur 
           rest <- many ((char ',') >>recur) 
           _<- char ')'
           return (Super r1 (r2:rest))
prim :: Parser Recur
prim = do _<- char '['
          r1 <- recur
          _<- char ','
          r2 <- recur 
          _<- char ']'
          return (Prim r1 r2)

mini :: Parser Recur
mini = do _<- char '{'
          r1 <- recur
          _<- char ','
          r2 <- integer 
          _<- char '}'
          return (Mini r1 r2)
expret::Parser (String,Recur)
expret = do str<-iden
            _<- char '='
            r<-recur
            _<-char ';'
            return (str,r)

system::Parser System
system = do res<-many1 expret
            eof
            return (res)

-- ---------------------������ ���� -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)
  , ("const0v2", Super Zero [Sel 2 1])
  , ("const0v3", Super Zero [Sel 3 1])
  , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
  , ("const2", Super Succ [Super Succ [Zero]]) 
  , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3])) 
  , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
  , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
  , ("subtract1", Prim Zero (Sel 2 1))  
  , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
  , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
  , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
  , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
  , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
  ]

syst2 = [("f1", Super Succ [Zero]),("f2", Super Succ [Name "f2"])]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
   \  const1v2 = (a1 : (z1 : s21));  \n\
   \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
   \  multiplication = [z1 , (addition: s33,s31)]; \n\
   \  notSignum = [(a1:z1),(z1:s21)];\n\
   \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
   \  subtractionRev = (subtraction : s22, s21);\n\
   \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
   \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
   \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"