module Radix where

import Prelude hiding (and, or)

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

data Bit = Zero | One
         deriving (Eq, Show)

type RadixTree = Tree Bit

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

-- 2
sizeIT :: IntTree -> Int
sizeIT Empty = 1
sizeIT (Leaf _) = 4
sizeIT (Node _ l r) = 12 + sizeIT l + sizeIT r 

sizeRT :: RadixTree -> Int
sizeRT (Leaf _) = 1
sizeRT (Node _ l r) = 8 + sizeRT l + sizeRT r

--
-- NOTE: The above import Prelude hiding (and, or) 
-- will allow you to name these two functions without
-- a name clash
--
-- 1
and :: Bit -> Bit -> Bit
and One One = One 
and _   _   = Zero 

or :: Bit -> Bit -> Bit
or Zero Zero = Zero 
or _    _    = One 

-- 3
binary :: Int -> BitString
binary x = bin x []
  where 
    bin :: Int -> [Int] -> [Int]
    bin 0 acc 
      | null acc = [0]
      | otherwise = acc
    bin rem acc = bin q (r:acc)
      where (q, r) = quotRem rem 2

insert :: BitString -> RadixTree -> RadixTree
insert [] t = case t of 
  (Node x l r) -> (Node One l r )
  (Leaf x) -> (Leaf One)
insert (b:bs) (Node x l r) = case b of 
  0 -> Node x (insert bs l) r
  1 -> Node x l (insert bs r)
insert bs (Leaf x) = insert bs (Node x (Leaf Zero) (Leaf Zero))


buildRadixTree :: [Int] -> RadixTree
-- buildRadixTree [] = Leaf Zero 
-- buildRadixTree (x:xs) = insert (binary x) (buildRadixTree xs)
buildRadixTree = foldr f (Leaf Zero)
  where 
    f :: Int -> RadixTree -> RadixTree
    f x acc = insert (binary x) acc

checkVal :: RadixTree -> Bit 
checkVal (Leaf x) = x 
checkVal (Node x _ _) = x 

member :: Int -> RadixTree -> Bool
member n = trav bString 
  where 
    bString = binary n 
    trav :: BitString -> RadixTree -> Bool 
    trav [] t = case checkVal t of 
      Zero -> False 
      One -> True 
    trav (b:bs) (Node x l r) = case b of 
      0 -> trav bs l 
      1 -> trav bs r 
    trav _ _ = False
union :: RadixTree -> RadixTree -> RadixTree
union 

intersection :: RadixTree -> RadixTree -> RadixTree
intersection
  = undefined

-- CONCLUSION: The break-even point is xxx.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node Zero (Leaf One)
               (Node One (Leaf Zero)
                          (Node One (Node Zero (Leaf One)
                                                 (Leaf Zero))
                                     (Leaf One)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node Zero (Node Zero (Leaf One)
                            (Node One (Leaf Zero) (Leaf One)))
                (Leaf One)
