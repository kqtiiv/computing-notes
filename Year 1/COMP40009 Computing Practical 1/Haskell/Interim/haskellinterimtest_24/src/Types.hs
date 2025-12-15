module Types where

type Operator = Char

data Token = TNum Int | TVar String | TOp Operator
             deriving (Eq, Show)

data Expr = ENum Int | EVar String | EApp Operator Expr Expr
            deriving (Eq, Show)

type Precedence = Int

data Associativity = L | N | R deriving (Eq, Show)

type ExprStack = [Expr]

type OpStack = [Operator]
