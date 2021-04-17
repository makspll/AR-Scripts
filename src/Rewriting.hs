module Rewriting where
import Common (Exp (Var, Lit, Func, Op, UnOp, None))
import Data.List ( sortOn, delete, mapAccumL, nub )
import Unification (unify)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Substitutions (Sub(..), subSet)
import qualified Data.Set.Internal as Data

import Debug.Trace ( trace, traceShow )
import Text.Printf (printf)
import Matching (match)


traced :: String -> a -> a
traced a b = b

data Rule =  Exp :=>: Exp
                deriving (Show,Eq)



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


applyRule :: Int -> Rule -> Data.Set Sub  -> Exp -> Exp
applyRule location r subs = dfsReplace $ applyRule' r subs location

applyRule' :: Rule -> Data.Set Sub -> Int -> Int -> Exp -> Exp
applyRule' (l :=>: r) subs location idx e@(Var a) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Lit a) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Func a bs) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(Op a op b) = if idx == location then subSet subs r else e
applyRule' (l :=>: r) subs location idx e@(UnOp op a) = if idx == location then subSet subs r else e


occurrences :: Rule -> Exp -> [(Int,Rule,Data.Set Sub)]
occurrences  rule@(l :=>: r) exp = let
                                subs = subexpressions exp
                                matches = map (\x -> (fst x,match l (snd x),snd x) ) subs
                              in [(i,rule,fromJust s) | (i,s,e) <- matches, isJust s]




fst3 (a,b,c) = a

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

-- DFS order based heuristic
heuristic :: [(Int,Rule, Data.Set Sub)] -> [(Int, Rule, Data.Set Sub)]
heuristic = sortOn fst3



allOccurrences :: [Rule] -> Exp -> [(Int,Rule, Data.Set Sub)]
allOccurrences rules e = foldl1 (++) $ map (`occurrences` e) rules

reduceToNormal :: [Rule] -> Exp -> [Exp]
reduceToNormal r e= nub (reduceToNormal' r e)

reduceToNormal' :: [Rule] -> Exp -> [Exp]
reduceToNormal' rules e =
        let
            applyRule3 (a,b,c) = applyRule a b c
            choices = allOccurrences rules e
        in
        case choices of
            [] -> [e]
            _ -> foldl (\acc n ->acc ++ reduceToNormal' rules (applyRule3 n e)) [] (heuristic choices)



factor1 :: Rule
factor1 = Op (Var "X") "&" (Op (Var "Y") "|" (Var "Z")) :=>: Op (Op (Var "X") "&" (Var "Y")) "|" (Op (Var "X") "&" (Var "Z"))

demoivre :: Rule
demoivre =UnOp "-" (Op (Var "X") "&" (Var "Y")):=>: Op (UnOp "-" (Var "X")) "|" (UnOp "-" (Var "Y"))
