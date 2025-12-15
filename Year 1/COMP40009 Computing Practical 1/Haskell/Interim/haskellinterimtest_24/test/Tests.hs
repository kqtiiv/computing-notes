import IC.TestSuite

import Parser
import Types

import Data.List

main :: IO ()
main = runTests tests

tests :: [TestGroup]
tests = [ testGroup "precedence" precedenceTests
        , testGroup "associativity" associativityTests
        , testGroup "supersedes" supersedesTests
        , testGroup "allVars" allVarsTests
        , testGroup "tokenise" tokeniseTests
        , testGroup "parseExpr (a)" parseExprTestsPartA
        , testGroup "parseExpr (b)" parseExprTestsPartB
        ]

precedenceTests =
  [
    precedence '*' --> 7,
    precedence '$' --> 0
  ]

associativityTests =
  [
    associativity '^' --> R,
    associativity '$' --> N
  ]

supersedesTests =
  [
    supersedes '+' '-' --> False,
    supersedes '*' '^' --> False,
    supersedes '^' '^' --> True,
    supersedes '*' '+' --> True
  ]

-- These are all sorted in lexographic order
allVarsTests =
  [
    (sort . allVars) "0" --> [],
    (sort . allVars) "x" --> ["x"],
    (sort . allVars) "x+y" --> ["x","y"],
    (sort . allVars) "2*(a+(b*c))" --> ["a","b","c"],
    (sort . allVars) "long+long+s" --> ["long", "s"],
    (sort . allVars) "long+s+long" --> ["long", "s"]
  ]

tokeniseTests =
  [
    tokenise "0" --> [TNum 0],
    tokenise "x" --> [TVar "x"],
    tokenise "x+1" --> [TVar "x",TOp '+',TNum 1],
    tokenise "+" --> [TOp '+'],
    tokenise "2*(x+1)" --> [TNum 2,TOp '*',TOp '(',TVar "x",TOp '+',TNum 1,TOp ')'],
    tokenise "2  * (\t \n x+   \t1)  " --> [TNum 2,TOp '*',TOp '(',TVar "x",TOp '+',TNum 1,TOp ')'],
    tokenise "longname" --> [TVar "longname"],
    tokenise "123" --> [TNum 123],
    tokenise "longname 123" --> [TVar "longname", TNum 123],
    tokenise "catch22" --> [TVar "catch22"]
  ]

parseExprTestsPartA =
  [
    parseExpr "0" --> ENum 0,
    parseExpr "x" --> EVar "x",
    parseExpr "x+1" --> EApp '+' (EVar "x") (ENum 1),
    parseExpr "x+2*3^2" --> EApp '+' (EVar "x") (EApp '*' (ENum 2) (EApp '^' (ENum 3) (ENum 2))),
    parseExpr "2^3^4" --> EApp '^' (ENum 2) (EApp '^' (ENum 3) (ENum 4)),
    parseExpr "2*3*4" --> EApp '*' (EApp '*' (ENum 2) (ENum 3)) (ENum 4),
    parseExpr "2+3-4" --> EApp '-' (EApp '+' (ENum 2) (ENum 3)) (ENum 4)
  ]

parseExprTestsPartB = [
    parseExpr "(1)" --> ENum 1,
    parseExpr "(((6)))" --> ENum 6,
    parseExpr "2+(3-4)" --> EApp '+' (ENum 2) (EApp '-' (ENum 3) (ENum 4)),
    parseExpr "(2^3)^4" --> EApp '^' (EApp '^' (ENum 2) (ENum 3)) (ENum 4),
    parseExpr "(3+4)*(5+6)^12" --> EApp '*' (EApp '+' (ENum 3) (ENum 4)) (EApp '^' (EApp '+' (ENum 5) (ENum 6)) (ENum 12))
  ]
