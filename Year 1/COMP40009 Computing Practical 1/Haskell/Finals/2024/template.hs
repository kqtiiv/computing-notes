{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ViewPatterns #-}

-- > This test can be considered as a continuation of a previous tutorial on
-- > differentiation. Of course, integration in general is not as
-- > straightforward. The test only focuses on one major technique, namely the
-- > reverse chain rule.
-- >
-- > In my opinion, this test is not difficult, but there are many edge cases
-- > and it is very easy to miss out on some of them, espcially under time
-- > pressure.
-- >
-- > In practice, of course, the integration rules covered in the test is only
-- > a tiny fraction, and it is in general a very hard problem to solve.

module Int where

import GHC.Real
import Data.List
import Data.Maybe
import Control.Applicative

import Types
import Utilities
import Examples

import Data.Bifunctor

--
-- Universal assumptions/preconditions:
-- 1. All polynomials are in standard form with decreasing
--    powers of x
-- 2. 0 is represented by P [(0, 0)]; P [] is undefined for
--    the purposes of the exercise.
-- 3. All constants will be polynomials of the form
--    [(c, 0)], e.g. logarithms of constants and constant
--    powers will not appear.
-- 4. All computed integrals omit the constant of integration.
--

-------------------------------------------------
-- Part I (13 marks)

addP :: Polynomial -> Polynomial -> Polynomial
addP = foldr addP' 
    where 
        addP' :: Term -> Polynomial -> Polynomial
        addP' t@(c, e) p = case p2 of 
            [] -> p ++ [t]
            ((c', e'):ts) -> 
                if e'==e 
                then if c'+c==0 then p1++ts else p1++[(c'+c,e)]++ts 
                else p1++[t]++p2 
            where 
                (p1, p2) = span ((>e).snd) p


mulP :: Polynomial -> Polynomial -> Polynomial
mulP p1 = foldr mulP' []
    where 
        mulP' :: Term -> Polynomial -> Polynomial
        mulP' t@(c, e) p = addP (map (\(c', e')->(c*c', e+e')) p1) p 


sumP :: [Polynomial] -> Polynomial
sumP = foldr addP [] 

prodP :: [Polynomial] -> Polynomial
prodP = foldr mulP [(1,0)]

diffT :: Term -> Term
diffT (c, 0) = (0, 0)
diffT (c, e) = (c*(e%1), e-1)

-- > The speç should specify the constant term to be zero!
intT :: Term -> Term
intT (0, 0) = (0, 0)
intT (c, e) = (c/(e'%1),e')
    where e' = succ e

diffP :: Polynomial -> Polynomial
diffP = map diffT

intP :: Polynomial -> Polynomial
intP = map intT 

-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr
diffE (P p) = P (diffP p)
diffE (Add e e') = Add (diffE e) (diffE e')
diffE (Mul e e') = Add (Mul e (diffE e')) (Mul (diffE e) e')
diffE (Pow e r) = Mul (Mul (toExpr r) (diffE e)) (Pow e (r-1))
diffE (Log e) = Mul (diffE e) (Pow e (-1))

--
-- Given
--
toExpr :: Rational -> Expr
toExpr n = P [(n, 0)]

isConstant :: Expr -> Bool
isConstant (P [(_, 0)]) = True
isConstant _ = False

simplifiedDiff :: Expr -> Expr
simplifiedDiff = simplify . diffE

printDiff :: Expr -> IO ()
printDiff = prettyPrint . simplifiedDiff

-------------------------------------------------
-- Part III (10 marks)

intE :: Expr -> Maybe Expr
intE (P p) = Just $ P (intP p)
intE (Add e e') = Add <$> intE e <*> intE e'
intE (Mul e e') 
    | isConstant e = Mul e <$> intE e'
    | isConstant e' = Mul e' <$> intE e
    | otherwise = applyICR e e' <|> applyICR e' e
intE e = applyICR (toExpr 1) e

applyICR :: Expr -> Expr -> Maybe Expr
applyICR g' fg = case factorise g' (diffE fg) of 
    Just coeff -> Just $ Mul (toExpr $ coeff/2) (Pow fg 2)
    _ -> case fg of 
        (Pow e r) -> do 
            coeff <- factorise g' (diffE e)
            pure $ case r of 
                -1 -> Mul (toExpr coeff) (Log e)
                _ -> Mul (toExpr (coeff / (r+1))) (Pow e (r+1))
        (Log e) -> do 
            coeff <- factorise g' (diffE e) 
            pure $ Mul (Mul (toExpr coeff) (Add (Log e) (toExpr (-1)))) e
        _ -> Nothing


factorise :: Expr -> Expr -> Maybe Rational
factorise (splitByCoeff -> (c, e)) (splitByCoeff -> (c', e')) 
    | e == e' = Just $ c/c'
    | otherwise = Nothing

splitByCoeff :: Expr -> (Rational, Expr)
splitByCoeff (simplify -> e) = case e of  
    Mul (P [(c,0)]) e2 -> (c, e2)
    _ -> (1, e) 
        



--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)