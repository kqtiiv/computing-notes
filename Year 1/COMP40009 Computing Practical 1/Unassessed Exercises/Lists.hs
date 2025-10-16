-- 3
pos :: Int -> [Int] -> Int
pos i xs = go xs 0 
    where 
        go (y:ys) !acc
            | y == i = acc
            | otherwise = go ys (1+acc)

--4 
twoSame :: [Int] -> Bool
twoSame