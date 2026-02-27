module Revision where 

import Prelude hiding (concat, reverse) 
import Data.List hiding (snoc, reverse, groupBy)
import Data.Maybe 
import Data.Set (Set) 
import Data.Map (Map)
import Data.Set qualified as Set 
import Data.Map qualified as Map 

-- (++) is expensive when the lh list is big 

-- (++) :: [a] -> [a] -> [a] 
-- [] ++ ys = ys 
-- (x:xs) ++ ys = x:(xs ++ ys)

-- do not write (xs ++ ys) ++ zs
-- write xs ++ ys ++ zs 

-- concat :: [[a]] -> [a] 
-- concat (xs:ys:zs) = foldr (++) [] (xs:ys:zs) 
--               = xs ++ foldr (++) [] (ys:zs)
--               = xs ++ ys ++ foldr (++) [] zs
--               = xs ++ ys ++ zs ++ [] -- goes through each list only once

-- concat (xs:ys:zs) = foldl' (++) [] (xs:ys:zs) -- DO NOT TO THIS
--                   = ((xs ++ ys) ++ zs) ++ []

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving Show

-- flatten :: Tree a -> [a] 
-- -- flatten (Leaf x) = [x] 
-- -- flatten (Fork lt rt) = flatten lt ++ flatten rt 

-- flatten t = flatten' t []
--     where 
--         flatten' :: Tree a -> [a] -> [a] 
--         flatten' (Leaf x) = (x:)
--         flatten' (Fork lt rt) = (flatten' lt . flatten' rt) 

-- snoc :: [a] -> a -> [a] 
-- snoc xs x = xs ++ [x]

-- reverse :: [a] -> [a] 
-- reverse = foldr go [] 
--     where 
--         go x acc = (acc++x) 

-- this causes A LOTTT of snocs, which is bad, so use (:) and reverse
-- reverse = foldr (flip snoc) []
-- reverse = foldl' (flip (:)) []

-- HOWEVER if you just want to use snoc once, just use snoc!!!!

-- addPairs :: [(Int, Int)] -> [Int] 
-- addParis ys = zipWith (+) ls rs 
--     where (ls, rs) = unzip ys 
-- addPairs = zipWith (uncurry (+))

-- unions' :: Eq a =>[[a]] -> [a] 
-- unions' = foldr union [] 

-- unions'' :: Eq a =>[[a]] -> [a] 
-- unions'' = nub . concat . map nub -- nub is O(n^2)

-- this is better if we have ordered data 
-- unions''' :: Ord a => [[a]] -> [a] 
-- unions''' = nubOrd . concat . map nubOrd -- nubOrd is O(nlogn)

{- 
             Eq       Ord        Ord
             []       Set        Map 
insert       O(n)     O(logn)    O(logn) 
delete       O(n)     O(logn)    O(logn) 
member       O(n)     O(logn)    O(logn) 
elem         O(n)     O(logn)    O(logn) 
fromList 
-}

-- Sets may offer a nicer more efficient way of representing unique
-- values where insertion order doesn't matter 

-- REMEMBER THESE 
-- fromList 
-- empty 
-- insert 
-- member 
-- delete 

-- setUnion s1 s2 = foldr (Set.insert) s1 (Set.toList s2)

-- so... sets, trees, be comfortable with the idea....

-- perfect trees have the same depth across every leaf 
-- has 2^n elements if depth of the leaf is n
--     ^^^ he told us to remember this...

-- some VERY nice functions
-- map :: (a -> b) -> [a] -> [b] 
-- concatMap 

-- [y | x <- [1, 2, 3, 4], y <- [x, x*10, x*20]]
-- do x <- [1, 2, 3, 4]; [x, x*10, x*20]

-- and, or, all, any 

-- replicate :: Int -> a -> [a] 

-- catMaybes :: [Maybe a] -> [a] 
-- maybe we will have to use this??

-- ohhh so there will be monads where we use maybes, and then we will generate
-- a list of maybes, and then we use catMaybes to turn it into a list!!!

-- there will be a sort of tic tac toe board of lists of lists for cols and rows
-- WAIT THAT WILL WORK
-- so take a block and if the block fits perfectly, it will return a value or something
-- and if not it returns a nothing!!! 

-- sequence [Just 3, Just 2, Just 1] == Just [3, 2, 1]

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a] 
groupBy f = foldr go Map.empty 
    where 
        go x = Map.insertWith (++) (f x) [x]

toBalancedTree :: [a] -> Tree a 
toBalancedTree [x] = Leaf x 
toBalancedTree xs = Fork (toBalancedTree l) (toBalancedTree r)
    where (l, r) = splitAt (length xs `div` 2) xs


treeToSet :: Ord a => Tree a -> Set a 
-- treeToSet (Leaf x) = Set.singleton x 
-- treeToSet (Fork lt rt) = Set.union (treeToSet lt) (treeToSet rt)

treeToSet t = treeToSet' t Set.empty 
    where 
        treeToSet' (Leaf x)  = Set.insert x 
        treeToSet' (Fork lt rt)  = treeToSet' lt . treeToSet' rt 

isPerfect :: Tree a -> Bool 
isPerfect t = depth t /= Nothing
    where 
        depth :: Tree a -> Maybe Int 
        depth (Leaf _) = Just 0 
        depth (Fork lt rt) = do 
            depthL <- depth lt
            depthR <- depth rt 
            if depthL == depthR 
                then return $ depthL+1 
                else Nothing 

perfTreeSize :: Tree a -> Int 
perfTreeSize t = 2 ^ depth t
    where 
        depth (Leaf _) = 0 
        depth (Fork lt _) = 1 + depth lt 

getPerfDepth :: Tree a -> Maybe Int 
getPerfDepth (Leaf _) = Just 0 
getPerfDepth (Fork lt rt) = do 
    ld <- getPerfDepth lt 
    rd <- getPerfDepth rt 
    if ld == rd 
        then return $ 1+ld 
        else Nothing 

-- flatten the tree into a list of all subtrees 
-- but only keeps the ones that are perfect 
flattenPerf :: Tree a -> [(Int, Tree a)]
flattenPerf t = catMaybes $ (perfSub t [])
    where 
        perfSub :: Tree a -> [Maybe (Int, Tree a)] -> [Maybe (Int, Tree a)]
        perfSub st acc = case st of 
            Leaf _ -> depth:acc
            Fork lt rt -> depth:(perfSub lt $ perfSub rt acc)
            where 
                depth = do 
                    d <- getPerfDepth st 
                    return (d, st)

-- return a list of perfect subtrees at depth d
indexPerfTrees :: Ord a => Tree a -> Map Int [Tree a]
indexPerfTrees t = foldr groupBy' Map.empty sts 
    where 
        sts = flattenPerf t 
        groupBy' :: (Int, Tree a) -> Map Int [Tree a] -> Map Int [Tree a]
        groupBy' (d, t) = Map.insertWith (++) d [t]
          
isPowerOfTwo :: Int -> Bool 
isPowerOfTwo 1 = True 
isPowerOfTwo n 
    | odd n = False 
    | otherwise = isPowerOfTwo (n `div` 2)

listToPerf :: [a] -> Maybe (Tree a) 
listToPerf xs 
    | null xs = Nothing 
    | isPowerOfTwo len = Just $ build xs len
    | otherwise = Nothing 
    where 
        len = length xs 
        
        build :: [a] -> Int -> Tree a 
        build [x] _ = Leaf x 
        build xs n = 
            let half = n `div` 2 
                (l, r) = splitAt half xs 
            in Fork (build l half) (build r half)