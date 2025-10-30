module Lists(twoSame, nextWord) where
import Data.Set (Set, member, empty, insert)
import Data.Map (Map, (!), fromList)
import Data.Char (isSpace)
import Data.List (unfoldr, (\\))
import Prelude hiding (lcm)

-- 3
pos :: Char -> String -> Int
pos i xs = go xs 0 
    where 
        go :: String -> Int -> Int
        go (y:ys) !acc
            | y == i = acc
            | otherwise = go ys (1+acc)

--4 
-- this is inefficient O(n^2)
-- twoSame :: [Int] -> Bool
-- twoSame [] = False
-- twoSame (x:xs) = elem x xs ||twoSame xs 

twoSame :: [Int] -> Bool
twoSame xs = go xs empty
    where 
        go :: [Int] -> Set Int -> Bool
        go [] _ = False
        go (x:xs) seen = member x seen || go xs (insert x seen)

-- 5
rev :: [Int] -> [Int] 
-- rev [] = []
-- rev (x:xs) = rev xs ++ [x]

-- tail - here strictness is not needed, as the : operator will not build a big thunk chain - it is mainly used for calculations
rev xs = go xs []
    where
        go :: [Int] -> [Int] -> [Int]
        go [] acc = acc
        go (y:ys) acc = go ys (y:acc)

-- 6
isPre :: String -> String -> Bool
isPre [] _ = True
isPre (x:xs) (y:ys) = x==y && isPre xs ys

substring :: String -> String -> Bool
substring x [] = False
substring x y@(_:ys) = isPre x y || substring x ys

-- 7
-- transpose :: String -> String -> String -> String
-- transpose _ _ [] = []
-- transpose x y (z:zs) = x !! (pos z y) : transpose x y zs

-- tail 
-- transpose x y z = go z ""
--     where 
--         go :: String -> String -> String
--         go [] acc = acc
--         go (h:ts) acc = go ts (acc ++ [x !! (pos h y)]) 

-- the above uses (!!) and pos which are very inefficient, so try to avoid!

-- the lookUp takes O(n) and transpose O(n^2), so still not very efficient
-- lookUp :: Eq a => a -> [(a, b)] -> b 
-- lookUp a ((a', b):abs)
--     | a == a' = b 
--     | otherwise = lookUp a abs

-- transpose :: String -> String -> String -> String
-- transpose s key = go 
--     where 
--         table = zip key s 
--         go :: String -> String
--         go [] = []
--         go (c:cs) = lookUp c table : go cs 

-- using maps: O(nlogn)
transpose :: String -> String -> String -> String
transpose s key = go
    where
        table = fromList (zip key s) 
        go :: String -> String
        go [] = []
        go (c:cs) = table ! c : go cs


trimWhitespace :: String -> String
-- trimWhitespace [] = []
-- trimWhitespace s@(x:xs)
--     | isSpace x = trimWhitespace xs
--     | otherwise = s

-- tail recursion
-- trimWhitespace x = go x ""
--     where
--         go :: String -> String -> String
--         go "" acc = acc
--         go (y:ys) acc 
--             | isSpace y = go ys acc
--             | otherwise = go ys (acc++[y])

-- drop while method
trimWhitespace x = dropWhile isSpace x

nextWord :: String -> (String, String)
nextWord x = go "" x
    where 
        go :: String -> String -> (String, String)
        go a "" = (a, "")
        go a (h:ts) 
            | isSpace h = (a, ts)
            | otherwise = go (a++[h]) ts

-- or you could use inbuild func break
-- nextWord x = (h, ts)
--     where (h, ts) = break isSpace x

-- 10 here i used ++ which is not optimal as it is o(n) 
-- splitUp :: String -> [String]
-- splitUp x = go x []
--     where
--         go :: String -> [String] -> [String]
--         go "" acc = acc
--         go y acc  = go b (acc++[a])
--             where
--                 (a, b) = nextWord y 

-- here i can a:acc then reverse the list to keep the order which is more idiomatic
-- splitUp :: String -> [String]
-- splitUp x = reverse (go x [])
--     where
--         go :: String -> [String] -> [String]
--         go "" acc = acc
--         go y acc  = go b (a:acc)
--             where
--                 (a, b) = nextWord y  

primeFactors :: Int -> [Int] 
primeFactors n = go 2 n []
    where
        go :: Int -> Int -> [Int] -> [Int]
        go d n acc
            | n == d = n : acc
            | n `mod` d == 0 = go d (n `div` d) (d:acc)
            | otherwise = go (d+1) n acc

-- 12
hcf :: Int -> Int -> Int
hcf a b = product cf
    where
        cf = af \\ (af \\ bf)
        af = primeFactors a
        bf = primeFactors b

-- 13
lcm :: Int -> Int -> Int
lcm a b = product cf
    where
        cf = bf ++ (af \\ bf)
        af = primeFactors a
        bf = primeFactors b

-- list comp
qsort :: [Int] -> [Int]
qsort [] = []
qsort (l:ls) = subL ++ (l:subG)
    where
        subL = qsort [x | x <- ls, x <= l]
        subG = qsort [x | x <- ls, x > l]

-- 3
allSplits :: [a] -> [([a], [a])]
allSplits x = [splitAt n x | n <- [1..(length x - 1)]]

-- 4
prefixes :: forall a. [a] -> [[a]]
-- prefixes a = map fst (allSplits a) ++ [a] -- uses ++

-- recursion with list comprehension
-- prefixes [] = []
-- prefixes (x:xs) = [x] : [x: prefix | prefix <- prefixes xs]

-- prefixes [] = []
-- prefixes (x:xs) = map (x:) ([]: prefixes xs)

-- the map has the same shape as a fold as base case is empty and recursively appends to list
prefixes = foldr pp []
    where 
        pp :: a -> [[a]] -> [[a]]
        pp x ps = map (x:) ([]:ps)

substrings :: String -> [String]
substrings [] = []
substrings x@(_:xs) = prefixes x ++ prefixes xs 

perms :: [Int] -> [[Int]]
perms xs = [x : ps | x <- xs, ps <- perms (xs \\ [x])]

routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes start end edges 
    | start == end = [[end]]
    | otherwise = [start : route | (s, e) <- edges, s == start, route <- routes e end edges]

routes2 :: Int -> Int -> [(Int, Int)] -> [[Int]]
-- routes2 start end edges = go start []
--     where
--         go :: Int -> [Int] -> [[Int]]
--         go current visited
--             | current == end = [[end]]
--             | otherwise = [current : route | (s, e) <- edges, s == current, not (e `elem` visited), route <- go e (current:visited)]

-- with sets
routes2 start end edges = go start empty
    where
        go :: Int -> Set Int -> [[Int]]
        go cur visited 
            | cur == end = [[end]]
            | otherwise = [cur : route | (s, e) <- edges, s == cur, 
                           not (member e visited), 
                           route <- go e (insert cur visited)]


-- useful functions imported from libraries:
-- Data.Set: member, insert, empty, fromList, union, intersection, difference, size 
-- Used to remove duplicates from a list
-- chicking membership efficiently (O(log n))
-- set operations like union, intersection, difference

-- Data.Map: (!), fromList, insert, member, empty, lookup, keys, elems, delete
-- Storing assitiations, counting frequencies, memoisation

-- Data.Char: isSpace, isAlpha, isDigit, toUpper, toLower

-- Data.List: unfoldr, foldl', foldr, intersperse, intercalate, nub, sort, group, (\\), partition, find, elem, transpose
-- unfoldr: generating lists
-- foldl' - almost always prefer foldl' to foldl/foldr as it is strict and avoids building up large thunks
-- good for building up a single result like sum, product, count 
-- foldr - good for lazy/short-circuit evaluations, as it terminates early for infinite lists

-- foldl' examples
-- sum = foldl' (+) 0
-- product = foldl' (*) 1
-- length = foldl' (\acc _ -> acc + 1) 0
-- reverse = foldl' (flip (:)) []
-- maximum = foldl1' max 

-- foldr examples
-- map f = foldr (\x acc -> f x : acc) []
-- filter p = foldr (\x acc -> if p x then x : acc else acc) []
-- any p = foldr (\x acc -> p x || acc) False
-- all p = foldr (\x acc -> p x && acc) True
-- concat = foldr (++) []

-- Rule of Thumb for Exams:

-- Building a list? → foldr
-- Calculating a number? → foldl'
-- Short-circuit logic? → foldr
-- When in doubt? → foldl' (safer default)

-- puts an element in between every element of a list
-- intersperse ',' "abc"           -- "a,b,c"
-- intersperse 0 [1,2,3]           -- [1,0,2,0,3]
-- intersperse " " ["hello","world"] -- ["hello"," ","world"]

-- joins a list of lists together with a given separator
-- intercalate ", " ["apple","banana","cherry"]  -- "apple, banana, cherry"
-- intercalate " " ["Hello","world"]             -- "Hello world"
-- intercalate [0] [[1,2],[3,4],[5]]            -- [1,2,0,3,4,0,5]

-- removes duplicates from a list - DO NOT USE!
-- nub [1,2,2,3,1,4]          -- [1,2,3,4]
-- nub "hello"                -- "helo"
-- nub []                     -- []
-- Warning: O(n²) complexity. For better performance, use Data.Set
-- nub' = toList . fromList  -- O(n log n)

-- SORT A LIST 
-- sort [3,1,4,1,5]           -- [1,1,3,4,5]
-- sort "haskell"             -- "aehklls"
-- sort ["zebra","apple"]     -- ["apple","zebra"]

-- sortOn length ["hi","hello","bye"]  -- ["hi","bye","hello"]

-- groups consecutive identical elements into sublists
-- group [1,1,2,2,2,3,1,1]    -- [[1,1],[2,2,2],[3],[1,1]]
-- group "aabbbaac"           -- ["aa","bbb","aa","c"]
-- group [1,2,3]              -- [[1],[2],[3]]

-- -- Count frequencies
-- frequencies xs = map (\g -> (head g, length g)) (group (sort xs))
-- frequencies "hello"  -- [('e',1),('h',1),('l',2),('o',1)]

-- -- Find duplicates
-- duplicates xs = [head g | g <- group (sort xs), length g > 1]
-- duplicates [1,2,2,3,3,3,4]  -- [2,3]

-- -- Run-length encoding
-- encode xs = [(head g, length g) | g <- group xs]
-- encode "aaabbbaac"  -- [('a',3),('b',3),('a',2),('c',1)]

-- SPLIT LIST 
-- partition even [1,2,3,4,5,6]   -- ([2,4,6], [1,3,5])
-- partition (>3) [1,2,3,4,5]     -- ([4,5], [1,2,3])
-- partition isUpper "HeLLo"      -- ("HLL", "eo")