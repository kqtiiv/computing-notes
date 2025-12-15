module Types where 

--1 
data Nat = Zero | Succ Nat 
nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat 
add x Zero = x 
add x (Succ y) = Succ (add x y)

mult :: Nat -> Nat -> Nat 
mult x Zero = Zero 
mult x (Succ y) = add x (mult x y)

--2
-- data Tree = Leaf Int | Node Tree Int Tree deriving (Show)
-- occurs :: Int -> Tree -> Bool 
-- occurs n (Leaf x) = x == n 
-- occurs n (Node l x r) = n == x || occurs n l || occurs n r 

-- occurs :: Int -> Tree -> Bool 
-- occurs n (Leaf x) = x == n 
-- occurs n (Node l x r) = case compare n x of 
--     LT -> occurs n r 
--     EQ -> n == x
--     GT -> occurs n l 

--3
data Tree = Leaf Int | Node Tree Tree 
numLeaves :: Tree -> Int 
numLeaves (Leaf _) = 1 
numLeaves (Node l r) = numLeaves l + numLeaves r

balanced :: Tree -> Bool 
balanced (Leaf _) = True 
balanced (Node l r) = numLeaves l == numLeaves r

balance :: [Int] -> Tree 
balance [l] = Leaf l 
balance ls = Node (balance l) (balance r)
    where 
        (l, r) = splitAt (numLeaves `div` 2) ls 
        numLeaves = length ls

data Prop = Const Bool 
          | Var Char 
          | Not Prop 
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop 
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool 
