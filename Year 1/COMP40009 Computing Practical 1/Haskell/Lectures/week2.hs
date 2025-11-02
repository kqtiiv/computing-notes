module Week2 (Day(..)) where -- exporting modles
-- the (..) specifies that i want to use EVERY data dype
-- i can also do (Mon, Tue) if i only want to use monday and tuesday

-- modules do not export its imports, so you would have to export it yourself if you want to
> module Week2 (module Day) where
> import Day

data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun

import Data.Char (chr, ord) -- importing functions from packages
import Prelude hiding (zip, unzip)-- always automatically imported
-- only reason why you would want to import it is if you want to remove functions from it

import Data.Char qualified as Katie
-- this does not introduce every single function into our namespace
-- we would have to define it as below
-- 
ord = Katie.ord

-- try not to use the !! operator, as it is an expensive function

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _      _      = []

zip (1:2:3:[]) (True:False:[]) = 
    (1, True) : zip (2:3:[]) (False:[]) =
    (1, True) : (2, False) : zip (3:[]) [] =
    (1, True) : (2, False) : [] =
    (1, True) : (2, False)

> 7 :: Num a -> a -- 7 has type a, but a is constrained by Num

-- Num has types Integral (Int, Integer, Word)+(div, mod, quot, rem, fromIntegral) and Fractional (Double, Flaot)+((/), recip), which conains the type Floating (Couble, Float)+(pi, exp, log, sin, cos)
-- Real goes to Rational

> recip :: Fractional a => a -> a
> recip n = 1/n 

> recip (7.0 :: Double) 
> recip@Double 7

> unzip :: forall a b. [(a, b)] -> ([a], [b]) -- writing forall braings tyope a and b into scope
> unzip [] = ([], []) 
> unzip ((x, y), xys) = (x:xs, y:ys)
>   where
>       xs :: [a] -- this a is the same a as above, as we defined it with forall
>       ys :: [b] -- if we did not define it above, haskell would add forall b. [b] here, which would not be the same b as the above
>       (xs, ys) = unzip xys

-- in other cases, we do not need to worry about forall, as haskell automatically handles it for us