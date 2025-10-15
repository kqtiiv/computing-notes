module Functions(addDigit, fToC, distance, triangleArea, turns, isPrime, fact, perm, goldenRatio) where

-- 1
addDigit :: Int -> Int -> Int 
addDigit x y = x * 10 + y 

-- 2
fToC :: Float -> Float
fToC t = t * 5/9 + 32

-- 3
type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

--4
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea p1 p2 p3 = sqrt (s*(s-a)*(s-b)*(s-c))
    where
        a = distance p1 p2
        b = distance p2 p3
        c = distance p1 p3
        s = (a+b+c)/2

--5
turns :: Float -> Float -> Float -> Float
turns start end r = dist / area 
    where 
        dist = ( end - start ) * kmToM
        area = pi * r^2
        kmToM = 1000

--6
isPrime :: Int -> Bool
isPrime a = a > 1 && null [x | x <- [2..xs], x `mod` x == 0]
    where xs = floor (sqrt (fromIntegral a))

-- 7
fact :: Integer -> Integer
fact n = go n 1 
    where 
        go :: Integer -> Integer -> Integer
        go 0 acc = acc
        go x !acc = go (x-1) (x*acc)

-- 8
perm :: Integer -> Integer -> Integer
perm n r = go n 1
    where
        go :: Integer -> Integer -> Integer
        go x !acc 
            | x == n - r = acc
            | otherwise = go (x-1) (x*acc)

-- 9 because this includes div, i must start from the smallest denominator, to avoid the fraction having remainders
choose :: Integer -> Integer -> Integer
choose n r = go 1 1
    where 
        go :: Integer -> Integer -> Integer
        go x !acc 
            | x > r = acc
            | otherwise = go (x+1) (acc*(n-x+1) `div` x)

-- 10 this is a tail recursive function itself, so you do not need to make it tail recursive

remainder :: Integer -> Integer -> Integer
remainder x y 
    | x < y     = x 
    | otherwise = remainder (x-y) y

-- 11
quotient :: Integer -> Integer -> Integer
quotient x y = go x 0
    where 
        go :: Integer -> Integer -> Integer
        go n !acc 
            | n < y = acc
            | otherwise = go (n-y) (acc+1)

-- 12
-- REMEMBER TO NOT USE MAGIC NUMBERS!!
binary :: Int -> Int
binary n 
    | n < base = n 
    | otherwise = r + 10 * binary q
        where 
            base = 2
            (q, r) = quotRem n base 

nary :: Int -> Int -> Int
nary n x 
    | x < n = x
    | otherwise = r + 10 * nary n q
        where
            (q, r) = quotRem x n 

-- 13
add :: Int -> Int -> Int
add x 0 = x
add x y = add (succ x) (pred y) 

-- 14
larger :: Int -> Int -> Int
-- larger x 0 = x 
-- larger 0 y = y 
-- larger x y = succ (larger (pred x) (pred y))

-- tail recursive vers
larger x y = go x y 0
    where
        go :: Int -> Int -> Int -> Int 
        go 0 _ acc = y
        go _ 0 acc = x
        go x' y' !acc = go (pred x') (pred y') (succ acc)

-- 14
chop :: Int -> (Int, Int)
chop x = go 0 x
    where 
        go :: Int -> Int -> (Int, Int)
        go q r
            | r < removeInit   = (q, r)
            | otherwise = go (q+addLast) (r-removeInit)
                where 
                    removeInit = 10
                    addLast = 1

-- 15
concatenate :: Int -> Int -> Int
concatenate x 0 = x 
concatenate x y = addDigit ( concatenate x i ) l
    where (i, l) = chop y

-- 16
fib :: Int -> Int
fib n = fib' 0 1 n 
    where 
        fib' :: Int -> Int -> Int -> Int
        fib' p1 p2 0 = p2
        fib' p1 !p2 !n' = fib' p2 (p1+p2) (n'-1)

-- 17
goldenRatio :: Float -> Float
goldenRatio e = go 1 2 1
    where 
        go :: Int -> Int -> Float -> Float
        go !p1 !p2 !prevRatio 
            | abs (ratio-prevRatio) < e = ratio
            | otherwise = go p2 (p1+p2) ratio
            where ratio = fromIntegral p2/fromIntegral p1
