module Parser where

import Data.List
import Data.Maybe
import Data.Char

import Types
import Examples

-------------------------------------------------------------------

-- The following are given...

ops :: [Operator]
ops = ['+', '-', '*', '/', '^', '(', ')', '$']

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), (')',(1,N)), ('+',(6,L)),
           ('-',(6,L)), ('*',(7,L)), ('/',(7,L)), ('^',(8,R))]

stringToInt :: String -> Int
stringToInt = read

prettyExpr :: Expr -> String
prettyExpr (ENum n) = show n
prettyExpr (EVar s) = s
prettyExpr (EApp op e e') = "(" ++ prettyExpr e ++ [op] ++ prettyExpr e' ++ ")"

-------------------------------------------------------------------

precedence :: Operator -> Precedence
-- Pre: the operator has a binding in opTable
precedence = undefined

associativity :: Operator -> Associativity
-- Pre: the operator has a binding in opTable
associativity = undefined

supersedes :: Operator -> Operator -> Bool
supersedes = undefined

tokenise :: String -> [Token]
-- Pre: The input string is a well-formed expression
tokenise = undefined

allVars :: String -> [String]
allVars = undefined

--
-- This function is given
--
parseExpr :: String -> Expr
-- Pre: The input string is a well-formed expression
parseExpr s = parse (tokenise s) [] ['$']

parse :: [Token] -> ExprStack -> OpStack -> Expr
-- Pre: The tokens and stacks collectively represent a
--      valid parser state.
parse = undefined
