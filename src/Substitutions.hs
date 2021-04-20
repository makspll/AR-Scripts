
module Substitutions where
import Common ( Exp(..) )
import qualified Data.Set as Set
import Control.Monad (msum)

data Sub = Exp :\: Exp deriving (Ord, Show, Eq)


sub :: Sub -> Exp ->  Exp
sub (t :\: Var s) (Var a)
    | a == s = t
    | otherwise = Var a
sub s (Lit a) = Lit a
sub s (Func a xs) = Func a (map (sub s) xs)
sub s (Op a op b) = Op (sub s a) op (sub s b)
sub s (UnOp op a) = UnOp op (sub s a)


subSet :: Set.Set Sub -> Exp -> Exp
subSet subs exp
    | Set.size subs /= 0 = Set.foldr sub exp subs
    | otherwise = exp


subColides ::  Set.Set Sub-> Sub -> Bool
subColides o (s :\: source) = Set.member source vars
    where vars = getVars o

mergeSub :: Sub -> Set.Set Sub -> Set.Set Sub
mergeSub al be = nubSub $ Set.union a' b'
    where
          a' = Set.map (mergeSub' al) be
          b' = Set.filter (not.subColides be) $ Set.fromList [al]

mergeSub' :: Sub -> Sub -> Sub
mergeSub' subs (s :\: Var b) = sub subs s :\: Var b

nubSub :: Set.Set Sub -> Set.Set Sub
nubSub = Set.filter (\( t :\: b) -> t /= b )






getVar :: Sub -> Exp
getVar (a :\: b) = b

getVars :: Set.Set Sub -> Set.Set Exp
getVars = Set.map getVar

