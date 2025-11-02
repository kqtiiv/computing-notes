module TypeTypeclasses () where 

import Text.Printf (printf)

type Vertex = (Float, Float)

data Shape = Triangle Float Float Float 
           | Square Float
           | Circle Float 
           | Polygon [Vertex]

area :: Shape -> Float 
area (Triangle a b c) = sqrt (s * (s-a) * (s-b) * (s-c))
    where 
        s = (a+b+c)/2
area (Circle r) = pi * r^2
area (Square a) = a^2
area (Polygon (v1:v2:v3:vs)) = area (Triangle a b c) + area (Polygon (v1:v3:vs))
    where 
        calcLen :: Vertex -> Vertex -> Float 
        calcLen (x, y) (x', y') = sqrt ((x-x')^2 + (y-y')^2)
        a = calcLen v1 v2 
        b = calcLen v2 v3 
        c = calcLen v3 v1 
area (Polygon _) = 0

type Day = Int 
type Month = Int 
type Year = Int 
data Date = Date {
    day :: Day,
    month :: Month, 
    year :: Year 
} deriving (Show, Eq)

age :: Date -> Date -> Int 
age birth cur 
    | month birth < month cur = year cur - year birth 
    | month birth > month cur = year cur - year birth -1 
    | day birth < day cur = year cur - year birth -1 
    | otherwise = year cur - year birth 

-- data Tree = Leaf | Node Tree Tree deriving (Show, Eq)

-- makeTrees :: Int -> [Tree]
-- makeTrees 0 = [Leaf]
-- makeTrees n = [Node left right | i <- [0..n-1]
--               , left <- makeTrees i
--               , right <- makeTrees (n-i-1)
--               ]

--5
-- data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving (Show, Eq)

-- build :: [a] -> BinTree a
-- build [] = error "Cannot build an empty tree"
-- build [x] = Leaf x 
-- build xs = Node left right 
--     where 
--         n = length xs 
--         (l, r) = splitAt ((n+1) `div` 2) xs 
--         left = build l 
--         right = build r 

-- ends :: BinTree a -> [a] 
-- ends (Leaf x) = [x]
-- ends (Node left right) = ends left ++ ends right 

-- -- this reverses the list values for ends
-- swap :: BinTree a -> BinTree a 
-- swap (Leaf x) = Leaf x
-- swap (Node l r) = Node (swap r) (swap l)

data AmPm = Am | Pm deriving (Eq)
type Hour = Integer
type Minute = Integer 
data Time = Clock24 Hour Minute | Clock Hour Minute AmPm 

instance Eq Time where 
    (==) :: Time -> Time -> Bool 
    x == y = equalTime x y

instance Show AmPm where 
    show :: AmPm -> String
    show Am = "am"
    show Pm = "pm"

instance Show Time where 
    show :: Time -> String
    show (Clock hr min ampm) = case (hr, min, ampm) of 
        (12, 0, Am) -> "Midnight"
        (12, 0, Pm) -> "Midday"
        _ -> printf "%d:%02d%s" hr min (show ampm)
    show (Clock24 hr min) = printf "%d:%02d" hr min

to24 :: Time -> Time 
to24 (Clock hr min ampm) 
    | ampm == Am = Clock24 newHr min 
    | otherwise = Clock24 (newHr + 12) min
    where newHr = hr `mod` 12
to24 _ = error "Not in clock format"

equalTime :: Time -> Time -> Bool 
equalTime x@(Clock _ _ _) y@(Clock24 _ _) = to24 x == y
equalTime x@(Clock24 _ _ ) y@(Clock _ _ _) = x == to24 y
equalTime (Clock24 h m) (Clock24 h' m') = h == h' && m == m'
equalTime (Clock h m ap) (Clock h' m' ap') = h == h' && m == m' && ap == ap'

--7
--a
data Tree a b = Empty 
              | Node (Tree a b) a (Tree a b)
              | Leaf b 
              deriving (Show, Eq)

--b 
-- mapTree :: Tree a b -> (a -> c) -> (b -> d) -> Tree c d 
-- mapTree Empty _ _    = Empty 
-- mapTree (Leaf x) _ g = Leaf (g x)
-- mapTree (Node left x right) f g = Node (mapTree left f g) (f x) (mapTree right f g)

--c 
foldTree :: d                   -- base case for empty
         -> (b -> d)            -- function for leaf
         -> (a -> d -> d -> d)  -- function for node
         -> Tree a b -> d 
foldTree base f g tree = case tree of
    Empty             -> base 
    Leaf x            -> f x
    Node left x right -> g x (foldTree base f g left) (foldTree base f g right)

-- i
countLeaves :: forall a b. Tree a b -> Int
countLeaves = foldTree 0 (const 1) addLeaves
    where 
        addLeaves :: a -> Int -> Int -> Int 
        addLeaves _ l r = l + r 

-- ii
treeSum :: Tree Int Int -> Int 
treeSum = foldTree 0 (id) addNodes 
    where addNodes :: Int -> Int -> Int -> Int 
          addNodes x l r = x + l + r

--iii
lFlatTree :: forall a. Tree a a -> [a]
lFlatTree = foldTree [] (:[]) reduceNode 
    where
        reduceNode :: a -> [a] -> [a] -> [a]
        reduceNode x l r = l ++ [x] ++ r 

-- iv 
-- using ++
-- rFlatTree :: forall a. Tree a a -> [a] 
-- rFlatTree = foldTree [] (:[]) reduceNode 
--     where reduceNode :: a -> [a] -> [a] -> [a] 
--           reduceNode x l r = r ++ [x] ++ l

--using difference lists
rFlatTree :: forall a. Tree a a -> [a] 
rFlatTree tree = (foldTree (const []) dls reduceNode tree) []
    where
        dls :: a -> ([a]->[a])
        dls x = (x:)
        reduceNode :: a -> ([a]->[a]) -> ([a]->[a]) -> ([a]->[a]) 
        reduceNode x l r = r . (x:) . l

--v
syntaxTree :: Tree (Int -> Int -> Int) Int -> Int 
syntaxTree = foldTree 0 (id) reduceNode
    where 
        reduceNode :: (Int->Int->Int) -> Int -> Int -> Int 
        reduceNode f l r = f l r 

--vi 
mapTree :: forall a b c d. (a -> c) -> (b -> d) -> Tree a b -> Tree c d 
mapTree f g = foldTree (Empty) ((Leaf).g) mapNode
    where 
        mapNode :: a -> Tree c d -> Tree c d -> Tree c d
        mapNode x l r = Node l (f x) r