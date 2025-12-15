module Recusion where 
import Prelude hiding ((^))

-- 1
(^) :: Int -> Int -> Int 
_ ^ 0 = 1
a ^ b = a * (a ^ (b-1))

--2 
-- length [1, 2, 3] = length (1:2:3:[])
--                  = 1 + length (2:3:[])
--                  = 1 + 1 + length (3:[])
--                  = 1 + 1 + 1 + length []
--                  = 1 + 1 + 1 + 0
--                  = 3

-- drop 3 [1, 2, 3, 4, 5] = drop 3 (1:2:3:4:5:[])
--                        = drop 2 (2:3:4:5:[])
--                        = drop 1 (3:4:5:[]) 
--                        = drop 0 (4:5:[])
--                        = (4:5:[])
--                        = [4, 5]

-- init [1, 2, 3] = init (1:2:3:[]) 
--                = 1 : init (2:3:[])
--                = 1 : 2 : init (3:[])
--                = 1 : 2 : []
--                = [1, 2]

--3 
-- and :: [Bool] -> Bool 
-- and [] = True 
-- and (x:xs) = x && and xs 

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (x:xs) = x ++ concat xs 

-- replicate :: Int -> a -> [a]
-- replicate 0 _ = []
-- replicate n x = x : replicate (n-1) x 

-- (!!) :: [a] -> Int -> a 
-- (x:xs) !! 0 = x 
-- (x:xs) !! n = xs !! (n-1)

-- elem :: Eq a => a -> [a] -> Bool 
-- elem y [] = False 
-- elem y (x:xs) = y == x || elem y xs 

-- 4

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys 
merge x'@(x:xs) y'@(y:ys)
    | x > y = y: merge x' ys 
    | otherwise = x : merge xs y'

--5 
msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort l) (msort r)
    where (l, r) = splitAt (length xs `div` 2) xs 

--6 
sum' :: Num a => [a] -> a 
sum' [] = 0
sum' (x:xs) = x + sum' xs 

take' :: Int -> [a] -> [a] 
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs 

last' :: [a] -> a 
last' [x] = x 
last' (x:xs) = last' xs