module Fold(foldr, sum, concat, map) where
import Prelude hiding (foldr, sum, concat, map, and) 

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f k [] = k
foldr f k (x:xs) = f x (foldr f k xs) 

-- using this we can define 
sum :: [Int] -> Int
sum xs = foldr (+) 0 xs

concat :: [[a]] -> [a] 
concat xs = foldr (++) [] xs

map :: (forall a. a -> forall b. b) -> [a] -> [b]
map f xs = foldr g [] xs
    where 
        g :: a -> [b] -> [b] 
        g x ys = f x : ys 

-- law 
-- foldr f k . map g = foldr (f.g) k 

any :: (a -> Bool) -> [a] -> Bool
any p xs = (or . map p) xs 

and :: [Bool] -> Bool
and xs = foldr (&&) True xs 

all :: (a -> Bool) -> [a] -> Bool
all p xs = (and . map p) xs



-- all p xs = foldr ((&&).p) True xs 