module Unification where 
    
import Matching (dPair)
import qualified Data.Set as Set
import Common ( Exp(..) )
import Substitutions ( Sub(..), sub, mergeSub )



unify :: Exp -> Exp -> Maybe(Set.Set Sub)
unify a b = unify' a b Set.empty

unify' :: Exp -> Exp -> Set.Set Sub -> Maybe (Set.Set Sub)
unify' a b subs
    | a == b = Just subs
    | otherwise =
        let
            pair = dPair a b
        in
            case pair of
                Just (Var t1,t2)-> if not (occurs (Var t1) t2)
                        then unify' (sub (t2 :\: Var t1) a) (sub (t2 :\: Var t1) b) (mergeSub (t2 :\: Var t1) subs)
                        else Nothing
                Just (t1 ,Var t2)-> if not (occurs (Var t2) t1)
                        then unify' (sub (t1 :\: Var t2) a) (sub (t1 :\: Var t2) b) (mergeSub (t1 :\: Var t2) subs)
                        else Nothing
                Nothing -> Just subs
                _ -> Nothing


occurs :: Exp -> Exp -> Bool
occurs (Var a) (Lit b) = False
occurs (Var a) (Var b) = a == b
occurs (Var a) (Func f xs) =  any (occurs (Var a)) xs
occurs (Var a) (Op l op r) = occurs (Var a) l || occurs (Var a) r

