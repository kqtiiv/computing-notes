module Lists(twoSame, nextWord) where
import Data.Set (Set, member, empty, insert)
import Data.Map (Map, (!), fromList)
import Data.Char (isSpace)

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
        go a (h:ts) 
            | isSpace h = (a, ts)
            | otherwise = go (a++[h]) ts

-- or you could use inbuild func break
-- nextWord x = (h, ts)
--     where (h, ts) = break isSpace x

-- 10
splitUp :: String -> [String]
splitUp x = go x []
    where
        go :: String -> [String] -> [String]
        go "" acc = acc
        go y acc  = go b (acc++[a])
            where
                (a, b) = nextWord y 