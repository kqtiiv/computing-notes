module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _) = True 
isFun _ = False  

splitDefs :: [Binding] -> ([Binding], [Binding])
-- splitDefs = foldr splitDefs' ([],[]) 
--   where 
--     splitDefs' :: Binding -> ([Binding], [Binding]) -> ([Binding], [Binding])
--     splitDefs' b@(_, def) (fs, vs) 
--       | isFun def = (b:fs, vs)
--       | otherwise = (fs, b:vs)
    
splitDefs = partition (isFun.snd) 

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs e) = length $ filter (isFun.snd) bs
topLevelFunctions _ = 0

---------------------------------------------------------
-- Part II

-- this part would be sets

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union []

-- THERE IS A MINIMUM AMOUNT OF ARGUMENTS YOU CAN ADD TO FUNCTIONS


freeVars :: Exp -> [Id]
freeVars (Const x) = []
freeVars (Var x) 
  | x `elem` prims = []
  | otherwise = [x]
freeVars (App e es) = 
  union (freeVars e) (unionAll $ map freeVars es)
freeVars (Fun ids e) = freeVars e \\ ids
freeVars (Let bs (App (Var x) es)) = 
  union (freeVars $ lookUp x bs) (unionAll $ map freeVars es)
freeVars (Let bs e) = 
  freeVars e \\ (unionAll $ map (freeVars.snd) (snd $ splitDefs bs))

---------------------------------------------------------
-- Part III

-- data Exp = Const Int | 
--            Var Id | 
--            Fun [Id] Exp |
--            App Exp [Exp] |
--            Let [Binding] Exp 
--          deriving (Eq, Show)

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Let bs e) = undefined
  -- let (funs, rem) = splitDefs bs
  --     fvs' = freeVars -- the free vars of siblings vars
  --     build (id, f) = (id, fvs' ++ freeVars f) -- func to build id fv pair
  --     fvs = map build funs -- (id, fvs) of funs 
  -- in (map build funs)
buildFVMap _ = []

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions 
  = undefined

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' 
  = undefined