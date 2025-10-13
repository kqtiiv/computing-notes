module HigherOrderFunc(ords) where
import Data.Char (ord)

-- a higher order function is a function that takes in a function as an argument
-- e.g., suppose we have a function ord 
-- we want to convert a list of characters into a list of integers

ords :: [Char] -> [Int] 
ords [] = []
ords (x:xs) = ord x : ords xs

-- higher order, as it takes a function as an input
map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- so now we could've written
-- ords xs = map ord xs

-- alternatively, a list comprehension is a map
-- [f x | x <- xs] = map f xs

-- function composition
-- we want to combine 2 functions to make another

-- succ (ord 'a') = succ 97 = 98

cipher :: Char -> Int
cipher c = succ (ord c)

(.) :: (b->c) -> (a->b) -> (a->c)
(g . f) x = g (f x)

-- first class : treats functions as arguments in functions, so you do not have to make a function to apply different combinations of functions

-- map fusion

map g . map f = map (g . f)

-- the rhs is much more efficient, as you only need to compute the map function once

-- filters

-- we already seen this in list comprehensions
-- this allows us to write 

evens :: [Ints] -? [Ints] 
evens xs = [x | x <- xs, even x]

-- without a list comprehension

evens [] = []
evens (x:xs) 
    | even x = x : evens xs
    | otherwise = evens xs

filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

any :: (a-> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) = p x || any p xs

all :: (a->Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys 
zip _      _      = []

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _      _      = []

-- every operator has a type

-- so we can write

-- zipWith (+) xs ys
-- zipWith (,) xs ys

zip xs ys = zipWith (,) xs ys


unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y):xys) = (x:xs, y:ys) 
    where
        (xs,ys) = unzip xys

concat :: [[a]] -> [b] 
concat [] = []
concat (xs:xss) = xs ++ concat xss

concatMap :: (a->[b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (x:xs) = f x ++ concatMap f xs

-- or

concatMap f xs = (concat.map f) xs

-- there is an isomorphism between these functions

-- (a, b) -> c = a->(b->c)

curry :: ((a, b) -> c) -> (a->(b->c)) 
curry f x y = f (x, y)

uncurry :: (a->b->c) -> ((a, b) -> c)
uncurry g (x, y) = g x y