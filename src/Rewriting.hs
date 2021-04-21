module Rewriting where
import Common
    ( Exp(UnOp, Op, Func, Lit, Var, None),
      Sub(..),
      Critical(..),
      Rule(..),
      RuleOccurrence )
      
import Data.List ( sortOn, delete, mapAccumL, nub )
import Unification (unify)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Substitutions (subSet, sub)
import qualified Data.Set.Internal as Data

import Debug.Trace ( trace, traceShow )
import Text.Printf (printf)
import Matching (match)
import Data.Char (isDigit)
import qualified Data.Ord


traced :: String -> a -> a
traced a b = b




dfsFold :: (Int -> Exp -> c -> c) -> c -> Exp -> c
dfsFold f b e = snd (dfsFold' f e (0,b))

dfsFold' :: (Int -> Exp -> b -> b) -> Exp -> (Int,b) -> (Int,b)
dfsFold' f e@(Var a) (idx,accum) = (idx,f idx e accum)
dfsFold' f e@(Lit a) (idx,accum) = (idx,f idx e accum)
dfsFold' f e@(Func a bs) (idx,accum) = foldl (\res ne -> dfsFold' f ne (fst res + 1,snd res)) (idx, f idx e accum) bs
dfsFold' f e@(Op a op b) (idx,accum) = foldl (\res ne -> dfsFold' f ne (fst res + 1,snd res)) (idx, f idx e accum) [a,b]
dfsFold' f e@(UnOp op a) (idx,accum) = foldl (\res ne -> dfsFold' f ne (fst res + 1,snd res)) (idx, f idx e accum) [a]

dfsReplace :: (Int -> Exp -> Exp) -> Exp -> Exp
dfsReplace f e = snd (dfsReplace' f e 0)

dfsReplace' :: (Int -> Exp -> Exp) -> Exp -> Int -> (Int, Exp)
dfsReplace' f e@(Var a) idx = (idx, f idx e)
dfsReplace' f e@(Lit a) idx = (idx, f idx e)
dfsReplace' f e@(Func a bs) idx = let
                                    chlrden = foldl (\res ne -> res ++ [dfsReplace' f ne (firstIdxb res idx+ 1)] ) [] bs
                                    chldren_exps = map snd chlrden
                                    in (fst.last $ chlrden, f idx $ Func a chldren_exps )
dfsReplace' f e@(Op a op b) idx = let
                                    chlrden = foldl (\res ne -> res ++ [dfsReplace' f ne (firstIdxb res idx+ 1)] ) [] [a,b]
                                    chldren_exps = map snd chlrden
                                    in (fst.last $ chlrden, f idx $ Op (head chldren_exps) op (last chldren_exps))
dfsReplace' f e@(UnOp op a) idx = let
                                    chlrden = foldl (\res ne -> res ++ [dfsReplace' f ne (firstIdxb res idx+ 1)] ) [] [a]
                                    chldren_exps = map snd chlrden
                                    in (fst.last $ chlrden, f idx $ UnOp op (head chldren_exps) )

subexpressions :: Exp -> [(Int, Exp)]
subexpressions = sortOn fst.dfsFold (\i e a -> (i,e):a) []




lastIdx :: [(Int, a)] -> Int
lastIdx = fst.head

firstIdx :: [(Int, a)] -> Int
firstIdx = fst.last

lastIdxb :: [(Int, a)] -> Int -> Int
lastIdxb [] b = b
lastIdxb xs _ = lastIdx xs

firstIdxb :: [(Int, a)] -> Int -> Int
firstIdxb [] b = b
firstIdxb xs _ = firstIdx xs





-- finds occurence of rule in the given expression given the matching/unification process, excluding certain subexpressions based on filter
ruleOccurrencesWith :: Rule -> Exp -> (Exp -> Exp -> Maybe(Data.Set Sub)) -> (Int -> Exp -> Bool) -> [RuleOccurrence]
ruleOccurrencesWith  rule@(lo :=>: ro) exp f filter = let
                                subexps = subexpressions exp
                                standardizationSub =  standardizeVariables lo exp
                                srule@(l :=>: r) = subSet standardizationSub lo :=>: subSet standardizationSub ro
                                matches = map (\x -> (fst x,f l (snd x),snd x) ) subexps
                              in [(i,srule, fromJust s ) | (i,s,e) <- matches, isJust s, filter i e]


occurrences :: Rule -> Exp -> [RuleOccurrence]
occurrences  r exp = ruleOccurrencesWith r exp match (\i e -> True)

allOccurrences :: [Rule] -> Exp -> [RuleOccurrence]
allOccurrences rules e = foldl1 (++) $ map (`occurrences` e) rules


applyRule :: RuleOccurrence  -> Exp -> Exp
applyRule (location, r, subs) = dfsReplace $ applyRule' r subs location

applyRule' :: Rule -> Data.Set Sub -> Int -> Int -> Exp -> Exp
applyRule' (l :=>: r) subs location idx e@(Var a) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Lit a) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Func a bs) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Op a op b) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(UnOp op a) = if idx == location then subSet subs r else e




replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

-- DFS order based heuristic
heuristic :: [RuleOccurrence] -> [RuleOccurrence]
heuristic = sortOn (\(a,b,c) -> a)




reduceToNormal :: [Rule] -> Exp -> [(Exp,[RuleOccurrence] )]
reduceToNormal r e= nub (reduceToNormal' r e [])

reduceToNormal' :: [Rule] -> Exp -> [RuleOccurrence] -> [(Exp,[RuleOccurrence])]
reduceToNormal' rules e xs =
        let
            choices =  allOccurrences rules e
        in
        case choices of
            [] -> [(e,xs ++ [(-1,None :=>: None,Data.fromList [])])]
            _ -> foldl (\acc n ->  acc ++ reduceToNormal' rules (applyRule n e) (xs ++ [n])) [] (heuristic choices)

--traceShow ("<",e,"|||",applyRule3 n e,"|||",n,">") 

factor1 :: Rule
factor1 = (read "X & (Y | Z)" :: Exp) :=>:  (read "(X & Y) | (X & Z)" :: Exp)

demoivre :: Rule
demoivre = (read "-(X & Y)" :: Exp) :=>: (read "-X | -Y" :: Exp)

power :: Rule 
power = (read "X^0" :: Exp) :=>: (read "1" :: Exp)

power2 :: Rule 
power2 = (read "0^X" :: Exp) :=>: (read "0" :: Exp)

frule :: Rule 
frule = (read "f(f(X))" :: Exp) :=>: (read "g(X)" :: Exp)


getVars :: Exp -> Data.Set Exp
getVars = dfsFold getVars' (Data.fromList [])

getVars' :: Int -> Exp -> Data.Set Exp -> Data.Set Exp
getVars' i e a = case e of
                        (Var n)-> Data.union a (Data.fromList [Var n])
                        _      -> a


variableNames :: Data.Set Exp
variableNames = Data.fromList $ map (\x -> Var [x]) ['A'..'Z']

extendVariableName :: Exp -> Exp
extendVariableName (Var a)
  | isDigit $ last a = Var (init a ++ show ((read [last a] :: Int) + 1))
  | otherwise = Var (a ++ "1")

standardizeVariables :: Exp -> Exp -> Data.Set Sub
standardizeVariables source target = Data.fromList $ zipWith (\x y -> y:\:x) varsLList alternativeNames
    where
      varsL = getVars source
      varsLList = Data.toList varsL
      varsR = getVars target
      extendedNames = Data.map extendVariableName varsL
      newNames = variableNames
      allReplacableNames = Data.union extendedNames newNames
      intersection = Data.intersection (Data.union varsL allReplacableNames) varsR
      alternativeNames = sortOn (Data.Ord.Down . (\(Var x) -> x)) (Data.toList $ Data.difference (Data.union variableNames extendedNames) intersection)




nubCPairs :: [Critical] -> [Critical]
nubCPairs = nub.filter (\(a:<>:b) -> a /= b)

criticalPairs :: Rule -> Rule -> [Critical]
criticalPairs ruleL@(l1 :=>: r1 ) ruleR@(l2 :=>: r2) = nubCPairs (lPairs ++ rPairs)
    where 
      nonVariable i (Var x) = False 
      nonVariable i _ = True 

      occurencesLinR = ruleOccurrencesWith ruleL l2 unify nonVariable
      occurencesRinL = ruleOccurrencesWith ruleR l1 unify nonVariable
      rPairs = criticalPairs' ruleR occurencesLinR
      lPairs = criticalPairs' ruleL occurencesRinL


      criticalPairs' :: Rule -> [RuleOccurrence] -> [Critical]
      criticalPairs' wholeRule@(lhs1 :=>: rhs1) = let 
          wholeApplication (l :=>: r) (loc,pr,mgu )  = subSet mgu r
          nestedApplication (l :=>: r) (loc, partialRule@(lp :=>: rp),mgu) = subSet mgu $ applyRule (loc, partialRule, mgu) l
          pair wholeRule partialOccurence = wholeApplication wholeRule partialOccurence :<>: nestedApplication wholeRule partialOccurence
            in map (pair wholeRule) 

allCriticalPairs :: [Rule] -> [Critical]
allCriticalPairs rules = let 
  double = [criticalPairs r r2 | r2 <- rules , r <- rules ] 

  in nubCPairs $ concat double 