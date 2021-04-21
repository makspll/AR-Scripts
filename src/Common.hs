module Common where
import Text.Printf

import Text.ParserCombinators.ReadP(ReadP, satisfy, readP_to_S, many1, eof,char, chainl1,chainl,skipSpaces, between, sepBy, (+++), (<++), look, munch1, munch, skipMany, string, sepBy1, pfail)
import Data.Char (isAlpha, isUpper, isLower, isAlphaNum, isNumber)
import Control.Applicative ((<|>), Alternative (many))
import Data.Functor ( ($>) )
import qualified Data.Set as Data




data Exp =  None |
            Var String |
             Lit String |
             Func String [Exp]|
             Op Exp String Exp |
             UnOp String Exp
               deriving (Eq,Ord)


data Sub = Exp :\: Exp deriving (Ord, Show, Eq)

type RuleOccurrence = (Int,Rule,Data.Set Sub)


getPos :: RuleOccurrence -> Int
getPos (a,b,c) = a 
getRule :: RuleOccurrence -> Rule
getRule (a,b,c) = b


data Rule =  Exp :=>: Exp
                deriving (Show,Eq)

data Critical = Exp :<>: Exp
  deriving (Ord)

instance Show Critical where
  show (a :<>: b) = "<" ++ show a ++ "," ++ show b ++ ">"

-- commutative equals
instance Eq Critical where
    (a :<>: b) == (c :<>: d) =
       (a == c && b == d) ||
       (a == d && b == c)



showArgs :: [Exp] -> String
showArgs xs = showArgs' xs ""

showArgs' :: [Exp] -> String -> String
showArgs' (x:xs) [] = showArgs' xs (show x)
showArgs' xs s = foldl (\ s x -> printf "%s,%s" s (show x)) s xs

instance Show Exp where
    show None = " "
    show (Var a) = a
    show (Lit a) = a
    show (Func a b) = printf "%s(%s)" a (showArgs b)
    show (Op a op b) = printf "(%s %s %s)" (show a) op (show b)
    show (UnOp op a) = printf "%s%s" op (show a)

------- Parser 

data Option = Unify Exp Exp |
              Match Exp Exp |
              CriticalPairs [Rule] |
              Normalize [Rule] Exp |
              Error  deriving (Show)



isValidChar :: Char -> Bool
isValidChar = isAlphaNum

isValidOperator :: Char -> Bool
isValidOperator c = c `elem` ['+','-','&','|','/','%','@','*','!','^','>','<','#','~']

capital,lower,operator :: ReadP Char
capital = satisfy (\x -> isValidChar x && isUpper x)
lower = satisfy (\x -> isValidChar x && (isLower x || isNumber x))
operator = satisfy isValidOperator

sep :: ReadP String -> ReadP ()
sep s = do
    _ <- skipSpaces
    sep <- s
    _ <- skipSpaces
    return sep $> ()

literalIdentifier,varIdentifier :: ReadP String
literalIdentifier = do
    _ <- skipSpaces
    c <- lower
    i <- munch isValidChar
    _ <- skipSpaces
    return (c : i)

varIdentifier = do
    _ <- skipSpaces
    c <- capital
    i <- munch isValidChar
    _ <- skipSpaces
    return (c : i)


var,lit,func,expr,terminalExp :: ReadP Exp
expr = do
        a <- terminalExp `chainl1` binOp
        if a == None 
            then pfail 
        else 
            return a 

binOp :: ReadP (Exp -> Exp -> Exp)
binOp = do
     _ <- skipSpaces
     op <- operator
     _ <- skipSpaces
     return (`Op` [op])


unop :: ReadP Exp
unop = do
    _ <- skipSpaces
    op <- operator
    r <- terminalExp
    return ([op] `UnOp` r)

terminalExp = unop <|>
            between (char '(') (char ')') expr <|>
            var <|>
            lit <|>
            func

var = Var <$> varIdentifier

lit = Lit <$> literalIdentifier


func = do
    name <- literalIdentifier
    args <- between (char '(') (char ')') (sepBy expr $ sep $ string ",")

    return (Func name args)

rule :: ReadP Rule
rule = do
    _ <- skipSpaces
    e1 <- expr
    _ <- sep $ string ":=>:"
    e2 <- expr
    return (e1:=>:e2)




parserExp :: String -> [(Exp,String)]
parserExp =readP_to_S $  do
    e1 <- expr
    _ <- skipSpaces
    end <- eof
    return e1

instance Read Exp where
    readsPrec a = parserExp




unifyOption :: ReadP Option
unifyOption = do
                e1 <- string "unify" *> skipSpaces *> expr
                e2 <- skipSpaces *> sep (string ",") *> expr <* skipSpaces
                return (Unify e1 e2)
matchOption :: ReadP Option
matchOption = do
                e1 <- string "match" *> skipSpaces *> expr
                e2 <- skipSpaces *> sep (string ",") *> expr <* skipSpaces
                return (Match e1 e2)

criticalOption :: ReadP Option
criticalOption = do
                    rules <- string "criticalPairs" *> skipSpaces *> sepBy1 rule (sep $ string ":")
                    _ <- skipSpaces
                    return (CriticalPairs rules)

normalizeOption :: ReadP Option
normalizeOption =  do
                    rules <- string "normalize" *> skipSpaces *> sepBy1 rule (sep $ string ":")
                    e <- skipSpaces  *> sep (string ",") *> expr <* skipSpaces
                    return (Normalize rules e)


option :: ReadP Option
option = unifyOption <|>
                matchOption <|>
                criticalOption <|>
                normalizeOption

parser :: ReadS [Option]
parser = readP_to_S $ do
    options <- sepBy1 option (sep $ string ";")
    _ <- skipSpaces
    end <- eof <|> skipSpaces *> char ';' <* skipSpaces *> eof
    return options
