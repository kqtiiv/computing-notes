module Functions(addDigit, fToC, distance, triangleArea, turns, isPrime, fact, perm) where

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

-- 9
choose :: Integer -> Integer -> Integer
choose n r = go n 1
    where 
        go :: Integer -> Integer -> Integer
        go x acc 
            | x == n - r = acc
            | otherwise = go (x-1) !(acc*x/(r*(x-r)))