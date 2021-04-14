module Common where
import Text.Printf



data Exp = Var String |
             Lit String |
             Func String [Exp]|
             Op Exp String Exp |
             BinOp String Exp 
               deriving (Eq,Ord)




showArgs :: [Exp] -> String
showArgs xs = showArgs' xs ""

showArgs' :: [Exp] -> String -> String
showArgs' (x:xs) [] = showArgs' xs (show x) 
showArgs' xs s = foldl (\ s x -> printf "%s,%s" s (show x)) s xs

instance Show Exp where
    show (Var a) = a
    show (Lit a) = a
    show (Func a b) = printf "%s(%s)" a (showArgs b)
    show (Op a op b) = printf "(%s %s %s)" (show a) op (show b)
    show (BinOp op a) = printf "%s%s" op (show a)
