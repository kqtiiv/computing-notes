module HigherOrderFunc(applyFToListIfP) where
import Prelude hiding (all, any, map)
import Data.List((\\), foldl')

{- 1. Show how the list comprehension [f x | x ← xs,px] can be re-expressed
 using the higher-order functions map and filter. -}

applyFToListIfP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
applyFToListIfP f p xs = map f (filter p xs)

all :: (a -> Bool) -> [a] -> Bool
all f = (and . map f) 

any :: (a -> Bool) -> [a] -> Bool
any f = (or . map f) 

takeWhile' :: forall a. (a -> Bool) -> [a] -> [a] 
-- takeWhile f (x:xs)
--     | f x       = x : takeWhile f xs
--     | otherwise = []

takeWhile' f = foldr func [] 
    where 
        func :: a -> [a] -> [a] 
        func x acc 
            | f x = x : acc 
            | otherwise = []

dropWhile' :: Eq a => (a -> Bool)  -> [a] -> [a] 
-- dropWhile f (x:xs) 
--     | f x = dropWhile f xs
--     | otherwise = xs 

dropWhile' f = snd.span f

-- 3 
map :: forall a b. (a -> b) -> [a] -> [b] 
map f (x:xs) = foldr go [] (x:xs) 
    where 
        go :: a -> [b] -> [b]
        go x ys = f x : ys 

filter' :: forall a. (a->Bool) -> [a] -> [a]
filter' f = foldr func [] 
    where 
        func :: a -> [a] -> [a] 
        func x acc 
            | f x = x : acc 
            | otherwise = acc 

-- 4
dec2int :: [Int] -> Int 
dec2int xs = foldl' func 0 xs
    where 
        func :: Int -> Int -> Int 
        func acc x = x + 10*acc

--5 
-- sumsqreven = compose [sum, map (^2), filter even]
-- this is not valid list comprehension and compose does not exist

--6 
curry' :: ((a, b) -> c) -> a -> b -> c 
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y


-- foldr - follows the simple pattern of recursion with this structure

-- f [] = v 
-- f (x:xs) = x (?) f xs 

-- sum, product, or, and 

-- foldr (?) v [a, b, c] = foldr (?) v (a:b:c:[])
--                       = a (?) foldr (?) v (b:c:[])
--                       = a (?) (b (?) foldr (?) v (c:[]))
--                       = a (?) (b (?) (c (?) foldr (?) v ([])))
--                       = a (?) (b (?) (c (?) v))

-- foldl - tail recursive 

type Bit = Int 

bin2int :: [Bit] -> Int 
-- bin2int bits = sum [ 2^i | (b, i) <- zip bits [0..], b==1]
-- bin2int bits = sum [ b*w | (b, w) <- zip bits weights] 
--     where weights = iterate (*2) 1 
bin2int = foldr f 0 
    where 
        f :: Bit -> Int -> Int 
        f b acc = b + 2 * acc 

