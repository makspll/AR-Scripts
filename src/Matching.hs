module Matching where 
    
import Substitutions ( Sub(..), sub, mergeSub ) 
import Common ( Exp(..) )
import qualified Data.Set as Set
import Control.Monad ( msum )

match :: Exp -> Exp -> Maybe(Set.Set Sub)
match source target = match' source target Set.empty


match' :: Exp -> Exp -> Set.Set Sub -> Maybe (Set.Set Sub)
match' source target alpha
    | source == target = Just alpha
    | otherwise =
                    case pair of
                        Just (Var t1,t2) ->
                            let
                                newSub = (t2 :\: Var t1)
                                substituted = sub newSub source
                                merged = mergeSub newSub alpha
                                in
                                    match' substituted target merged
                        Nothing -> Just alpha
                        _ -> Nothing
        where
            pair = dPair source target

dPair :: Exp -> Exp -> Maybe (Exp, Exp)
dPair (Var a) (Var b)
    | a == b =  Nothing
    | otherwise = Just (Var a,Var b)
dPair (Lit a) (Lit b)
    | a == b =  Nothing
    | otherwise = Just (Lit a,Lit b)
dPair f1@(Func n as) f2@(Func n2 bs) = if n == n2 
                                        then msum $ zipWith dPair as bs 
                                        else Just (f1,f2)

dPair (Op a opl b) (Op d opr e) = msum [dPair a d,dPair b e]
dPair a b = Just (a,b)

