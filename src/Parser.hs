module Parser where

import Common(Exp (Var, Lit, Func, Op, BinOp))
import Text.ParserCombinators.ReadP(ReadP, satisfy, readP_to_S, many1, eof,char, chainl1,chainl,skipSpaces, between, sepBy, (+++), (<++), look, munch1, munch, skipMany)
import Data.Char (isAlpha, isUpper, isLower, isAlphaNum, isNumber)
import Control.Applicative ((<|>), Alternative (many))
import Data.Functor ( ($>) )

isValidChar :: Char -> Bool
isValidChar = isAlphaNum

isValidOperator :: Char -> Bool
isValidOperator c = c `elem` ['+','-','&','|','/','%','@','*','!']

capital,lower,operator :: ReadP Char
capital = satisfy (\x -> isValidChar x && isUpper x)
lower = satisfy (\x -> isValidChar x && (isLower x || isNumber x))
operator = satisfy isValidOperator


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

expr =  (terminalExp `chainl` binOp) (Lit " ")


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
    return ([op] `BinOp` r)

terminalExp = unop <|>
            between (char '(') (char ')') expr <|>
            var <|>
            lit <|>
            func 

var = Var <$> varIdentifier

lit = Lit <$> literalIdentifier

func = do
    name <- literalIdentifier
    args <- between (char '(') (char ')') (sepBy expr (char ','))
    return (Func name args)


data Option = Unify Exp Exp |
              Match Exp Exp deriving (Show) 
              
parser :: ReadS Option
parser = readP_to_S $ do
    option <- munch1 isValidChar 
    let (o,arg1,arg2) = case option of 
                    "unify" -> 
                        (0,expr <* char ',' ,expr)
                    "match" -> 
                        (1,expr <* char ',', expr)

    e1 <- arg1 
    e2 <- arg2 

    end <- eof
    return  (case o of 
                0 -> Unify e1 e2
                1 -> Match e1 e2)

