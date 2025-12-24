module Lists where 
import Data.List 

-- 1 
myLast :: [a] -> a 
myLast [x] = x 
myLast (x:xs) = myLast xs 

-- myLast' = foldr1 (const id)

-- myLast'' = foldr1 (flip const)

-- myLast''' = head . reverse

-- myLast'''' = foldl1 (curry snd)

--2 
myButLast xs = x
    where (_:x:_) = (reverse xs)

--3 
elementAt :: [a] -> Int -> a 
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1) 

-- 4 
myLength :: [a] -> Int 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--5 
myReverse :: [a] -> [a] 
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--6 
isPalindrome xs = xs == reverse xs

--7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs  

-- 8
compress :: Eq a => [a] -> [a] 
compress (x:x':xs) 
    | x == x' = compress (x':xs) 
    | otherwise = x: compress (x':xs)
compress xs = xs

--10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- group xs]