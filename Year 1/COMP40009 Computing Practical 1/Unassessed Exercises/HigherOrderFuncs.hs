module HigherOrderFuncs where
import Data.List (foldl', scanl')
import Data.Char (chr)
import Prelude hiding (zipWith, map, filter)

-- 1a
depunctuate :: String -> String
depunctuate = filter f 
    where 
        f :: Char -> Bool
        f c = not (elem c [',', '.', ':'])

-- 1b
makeString :: [Int] -> [Char]
makeString = map chr 

--1c
enpower :: [Int]-> Int
-- enpower [n] = n
-- enpower (n : ns) = enpower ns ^ n

enpower = foldr1 (flip (^))

-- 1d
revAll :: [[a]]-> [a]
-- revAll [] = []
-- revAll (x : xs) = reverse x ++ revAll xs

revAll = concatMap reverse

-- 1e
rev :: [a]-> [a]
-- rev xs = rev' xs []
--     where rev' [] ys = ys
--           rev' (x : xs) ys = rev' xs (x : ys)

rev = foldl' (flip (:)) []

dezip :: forall a b. [(a,b)]-> ([a],[b])
-- dezip [] = ([], [])
-- dezip ((x, y) : ps) = let (xs, ys) = dezip ps in (x : xs, y : ys)

dezip = foldr f ([], [])
    where 
        f :: (a, b) -> ([a], [b]) -> ([a], [b])
        f (x, y) (xs, ys) = (x:xs, y:ys)

-- 2
allSame :: [Int] -> Bool
allSame xs = and (zipWith (==) xs (tail xs))

-- 3
facts :: [Integer]
facts = scanl' (*) 1 [1..]

e :: Double
e = sum $ map (recip.fromInteger) $ take 10 facts

mystery = 1 : scanl (+) 1 mystery

--4 
--a
-- squash :: (a-> a -> b) -> [a] -> [b]
-- squash f (x:x':xs) = f x x' : squash f (x':xs)
-- squash _ _ = []

--b
squash :: (a-> a -> b) -> [a] -> [b]
squash f xs = zipWith f xs (tail xs)

--5
converge :: (a-> a -> Bool) -> [a] -> a 
converge f (x:y:xs)
    | f x y = x 
    | otherwise = converge f (y:xs)
converge _ [x] = x 


e2 :: Double
e2 = converge lim (sums $ map (recip . fromInteger) facts)
    where 
        lim :: Double -> Double -> Bool
        lim x y = abs (x - y) < 0.00001
        sums :: [Double] -> [Double]
        sums = scanl' (+) 0.0

limit :: (a-> a-> Bool) -> [a] -> [a]
limit f (x:y:xs)
    | f x y = [x]
    | otherwise = x : limit f (y:xs)
limit _ xs = xs

-- 7
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f 

-- 8
isElem :: Eq a => a -> [a] -> Bool
isElem = any . (==)

-- 9 
(<.>) :: (c->d) -> (a->b->c) -> (a->b->d)
-- f <.> g = \x y -> f (g x y)
(<.>) = (.).(.)

-- Examples:
-- lengthSum = length <.> (++)          -- lengthSum "hello" "world" = 10
-- notEqual  = not <.> (==)             -- notEqual 3 4 = True

-- 11
--a
zipWith :: forall a b c. (a->b->c) -> [a] -> [b] -> [c]
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys 
-- zipWith _ _      _      = []

-- zipWith f (x:xs) = matchYs
--     where matchYs :: [b] -> [c]
--           matchYs [] = []
--           matchYs (y:ys) = f x y : zipWith f xs ys
zipWith f xs = foldr cons nil xs 
    where cons :: a -> ([b] -> [c]) -> [b] -> [c]
          cons _ _ [] = []
          cons x next (y:ys) = f x y : next ys
          nil = const []

map :: forall a b. (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map xs 

map f = foldr func [] 
    where 
        func :: a -> [b] -> [b]
        func y acc = f y : acc 

filter :: forall a. (a -> Bool) -> [a] -> [a]
filter f = foldr func []
    where 
        func :: a -> [a] -> [a]
        func x acc 
            | f x = x : acc 
            | otherwise = acc

-- foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b 
-- -- foldl f acc [] = acc 
-- -- foldl f acc (x:xs) = foldl f (f acc x) xs 

-- foldl f z xs = foldr step id xs z 
--     where 
--         step :: a -> (b -> b) -> b -> b 
--         step x next acc = next (f acc x) 

-- foldr :: forall a b. (a -> b -> b) -> b -> [a] -> b 
-- foldr f z xs = foldl step id xs z 
--     where 
--         step :: (b -> b) -> a -> b -> b 
--         step next x acc = next (f x acc)

pipeline :: forall a. [a -> a] -> [a] -> [a]
pipeline = map . foldr (.) id