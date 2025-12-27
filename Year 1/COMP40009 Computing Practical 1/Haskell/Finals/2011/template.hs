import Data.Maybe
import Data.Char (chr)

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x = fromJust . lookup x

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp x d t = case lookup x t of 
    Nothing -> d 
    _ -> lookUp x t 

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp x t = [ k | (k, v) <- t, v==x]

occurs :: String -> Type -> Bool
occurs x t = case t of 
    TVar x' -> x'==x
    TFun t1 t2 -> occurs x t1 || occurs x t2 
    _ -> False 

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType e env = case e of 
    Number _ -> TInt 
    Boolean _ -> TBool 
    Id s -> lookUp s env 
    Prim s -> lookUp s primTypes
    Cond p e1 e2 -> case inferType p env of 
        TBool -> let t1 = inferType e1 env
                     t2 = inferType e2 env 
                 in (if t1==t2 then t1 else TErr)
        _ -> TErr
    app -> inferApp app env

inferApp :: Expr -> TEnv -> Type
inferApp (App f a) env = case t' of 
    (TFun t1 t2) -> if t1==t3 then t2 else TErr
    _ -> TErr
    where 
        t' = inferType f env 
        t3 = inferType a env

------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub s t@(TVar v) = tryToLookUp v t s 
applySub s (TFun t1 t2) = TFun (applySub s t1) (applySub s t2)
applySub s t = t

unify :: Type -> Type -> Maybe Sub
unify t t'
    = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs (p:ts) s = case p of 
    (TInt, TInt) -> unifyPairs ts s 
    (TBool, TBool) -> unifyPairs ts s 
    (TVar v, TVar v') -> if v == v' then unifyPairs ts s else Nothing
    (TVar v, t) -> if occurs v t then Nothing else unifyPairs (substitute [(v, t)] ts) ((v,t):s)
    (t, TVar v) -> if occurs v t then Nothing else unifyPairs (substitute [(v, t)] ts) ((v,t):s)
    (TFun t1 t2, TFun t1' t2') -> unifyPairs ((t1, t1'):(t2,t2'):ts) s 
    _ -> Nothing 
unifyPairs [] s = Just s

substitute :: [(String, Type)] -> [(Type, Type)] -> [(Type, Type)]
substitute sub = map (\(t1, t2) -> (apply t1, apply t2))
    where 
        apply = applySub sub

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType e = t
    where (_,t,_) = inferPolyType' e [] [ 'a':(show x) | x <- [1..]]
-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String]) -- (sub for any type vars in tenv, inferred type, )
inferPolyType' e@(Number _) env as = (env, inferType e env, as)
inferPolyType' e@(Boolean _) env as = (env, inferType e env, as)
inferPolyType' e@(Prim _) env as = (env, inferType e env, as)
inferPolyType' e@(Id x) env as = (env, inferType e env, as)
inferPolyType' (Fun x e) env (a:as) = 
    let (sub, t, ss) = inferPolyType' e ((x, TVar a):env) as
        te = if t == TErr then TErr else TFun (applySub sub (TVar a)) t
    in (sub, te, ss)
inferPolyType' (App f e) env (a:as) = 
    let (sub, t, ss) = inferPolyType' f env as 
        (sub', te, ss') = inferPolyType' e (updateTEnv env sub) ss 
        Just s = unify t (TFun te (TVar a))
        usub = combineSubs [s,sub',sub]
    in (usub, applySub usub (TVar a), ss')

-- inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
-- inferPolyType' 
--   = undefined
{-data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)-}
------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))
