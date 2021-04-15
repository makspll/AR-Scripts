module Rewriting where
import Common (Exp (Var, Lit, Func, Op, UnOp))
import Data.List ( sortOn )
import Unification (unify)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Substitutions (Sub(..), subSet)
import qualified Data.Set.Internal as Data

import Debug.Trace ( trace, traceShow )
import Text.Printf (printf)


traced :: String -> a -> a
traced a b = b

data Rule =  Exp :=>: Exp
                deriving (Show,Eq)





subexpressions :: Exp -> [(Int, Exp)]
subexpressions = sortOn fst.subexpressions' [] 0

subexpressions' :: [(Int, Exp)] -> Int -> Exp  -> [(Int, Exp)]
subexpressions' xs idx (Var a) = (idx, Var a) : xs
subexpressions' xs idx (Lit a) = (idx, Lit a) : xs
subexpressions' xs idx f1@(Func a bs) = foldl (\res ne-> subexpressions' res (lastIdx res + 1) ne) ((idx,f1) : xs) bs
subexpressions' xs idx (Op a op b) = let
                                        lhs = subexpressions' ((idx, Op a op b) : xs) (idx + 1) a
                                        rhs = subexpressions' xs (lastIdx lhs + 1) b
                                        in lhs ++ rhs
subexpressions' xs idx (UnOp op a) = subexpressions' ((idx, UnOp op a) : xs) (idx + 1) a

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

applyRule :: Rule -> Data.Set Sub-> Int -> Exp -> Exp
applyRule r subs idx a = snd $ applyRule' r subs idx  0 a


applyRule' :: Rule -> Data.Set Sub -> Int -> Int -> Exp -> (Int,Exp)
applyRule' (l :=>: r) subs it idx (Var a)
    | it == idx = (idx,subSet subs r)
    | otherwise = (idx,Var a)
applyRule' (l :=>: r) subs it idx (Lit a)
    | it == idx =  (idx,subSet subs r)
    | otherwise =  (idx,Lit a)
applyRule' rule@(l :=>: r) subs it idx f1@(Func a xs)
    | it == idx = (idx,subSet subs r)
    | otherwise =
            let
                folder :: [(Int,Exp)] -> Exp -> [(Int,Exp)]
                folder acum newE = acum ++ [applyRule' rule subs it (firstIdxb acum idx + 1) newE]
                result = foldl folder [] xs
                in (firstIdx result,Func a (map snd result))

applyRule' rule@(l :=>: r) subs it idx (UnOp op a)
    | it == idx = (idx,subSet subs r)
    | otherwise = let
                    (i, result) = applyRule' rule subs it (idx + 1) a
                  in (idx, UnOp op result)

applyRule' rule@(l :=>: r) subs it idx (Op a op b)
    | it == idx = (idx,subSet subs r)
    | otherwise = let
                    (lidx, lhs) = applyRule' rule subs it (idx + 1) a
                    (ridx, rhs) = applyRule' rule subs it (lidx + 1) b
                  in (ridx, Op lhs op rhs)



occurrences :: Rule -> Exp -> [(Int,Data.Set Sub,Exp)]
occurrences  (l :=>: r) exp = let
                                subs = subexpressions exp
                                matches = map (\x -> (fst x,unify l (snd x),snd x) ) subs
                              in [(i,fromJust s,e) | (i,s,e) <- matches, isJust s]

