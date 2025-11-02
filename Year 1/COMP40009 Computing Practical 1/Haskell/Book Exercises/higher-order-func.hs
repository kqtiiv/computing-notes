module HigherOrderFunc(applyFToListIfP) where
import Prelude hiding (all, any, takeWhile, dropWhile, map)

{- 1. Show how the list comprehension [f x | x ← xs,px] can be re-expressed
 using the higher-order functions map and filter. -}

applyFToListIfP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
applyFToListIfP f p xs = map f (filter p xs)

all :: (a -> Bool) -> [a] -> Bool
all f xs = (and . map f) xs

any :: (a -> Bool) -> [a] -> Bool
any f xs = (or . map f) xs

takeWhile :: (a -> Bool) -> [a] -> [a] 
takeWhile f (x:xs)
    | f x       = x : takeWhile f xs
    | otherwise = []

dropWhile :: (a -> Bool)  -> [a] -> [a] 
dropWhile f (x:xs) 
    | f x = dropWhile f xs
    | otherwise = xs 

map :: forall a b. (a -> b) -> [a] -> [b] 
map f (x:xs) = foldr go [] (x:xs) 
    where 
        go :: a -> [b] -> [b]
        go x ys = f x : ys 
        