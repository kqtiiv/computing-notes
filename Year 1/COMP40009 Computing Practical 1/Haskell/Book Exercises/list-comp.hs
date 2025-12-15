module ListCompr where 

import Prelude hiding (replicate)
import Data.Char (ord, chr, isLower)

find :: Eq a => a -> [(a, b)] -> [b]
find x ps = [y | (x', y) <- ps, x'==x]


count :: Eq a => a -> [a] -> Int 
-- count x xs = sum [ 1 | y <- xs, y==x]
count x = length . filter (==x)

lowers :: [Char] -> Int
lowers = length . filter (isLower)

let2int :: Char -> Int 
let2int c = ord c - ord 'a'

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char 
shift n c 
    | isLower c = int2let ((let2int c + n) `mod` alphaLen)
    | otherwise = c
    where 
        alphaLen = 26

encode :: Int -> String -> String 
encode n s = map (shift n) s

percent :: Int -> Int -> Float 
percent n m = (fromIntegral n / fromIntegral m) * 100

table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
         6.7, 7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

freqs :: String -> [Float]
freqs cs = [ percent (count c cs) (length cs)
           | c <- ['a'..'z'] ]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n s = drop n s ++ take n s

guessKey :: String -> Int
guessKey m = head $ positions (minimum chisqrs) chisqrs
    where 
        chisqrs = [ chisqr (rotate n os) table | n <- [0..25] ]
        os = freqs m

crack :: String -> String 
crack m = encode (negate $ guessKey m) m

--1 
sumOf100Sq :: Integer
sumOf100Sq = sum [ x ^ 2 | x <- [1..100]]

--2 
replicate :: Int -> a -> [a]
replicate n x = [ x | _ <- [1..n]]

--3 
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n]
                      , y <- [1..n]
                      , z <- [1..n]
                      , x^2 + y^2 == z^2
                      ]

--4
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factors x) == 2*x]

--5
pairs :: [(Int, Int)]
-- pairs = [ (x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

pairs = concat [ [ (x, y) | x <- [1, 2, 3]] | y <- [4, 5, 6]]

-- 6 
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x==x']

--7 
scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]

--8
