-- defining a datatype

-- data Cowboy where
--     Good::Cowboy
--     Bad::Cowboy
--     Ugly::Cowboy

data Cowboy = Good | Bad | Ugly

-- the same thing

-- tail recursion
-- functions are more efficient when they are tail recursive - they call itself with different arguments.

isPower2 :: Int -> Bool
isPower2 0 = False
isPower2 1 = True
isPower2 n
    | n `mod` 2 /= 0 = False
    | otherwise = isPower2 (n `div` 2)

-- produces efficient code as the isPower2 function only needs to call itself

add :: (Int, Int) -> Int
add (x, y) = x + y

-- making general functions

-- plus :: Int -> (Int -> Int) 
-- plus 1 = plusOne

-- this function produces a function

plus :: Int -> (Int -> Int) 
plus x = \ y -> x + y

-- a -> (b -> c) = a -> b -> c /= (a -> b) -> c

-- so far all funcs have been monomorphic
-- polymorphism allows us to vary the function for different types

identity :: forall a . a -> a
identity x = x

-- type application: monomorphise a function 

idInt :: Int -> Int
idInt = id @Int

-- this is a 1 parameter function that produces a 1 parameter function
-- power :: Int -> Int -> Int
-- power n 0 = 1
-- power n k
--     | even k = power n (k `div` 2) * power n (k `div` 2)
--     | otherwise = n * power n (k `div` 2) * power n (k `div` 2)

-- the problem is that we will repeat the computation power n (k `div` 2) twice
-- to avoid this, we name the clause and reuse the variable

power :: Int -> Int -> Int
power n 0 = 1
power n k 
    | even k = x * x
    | otherwise = n * x * x
    where 
        x = power n (k `div` 2) -- tells the program to store it in cache
        -- this variable is only in scope for the clause it is defined in
        -- Haskell remembers this value and reuses itss


-- lists: ordered sequence of values of the same type (homogenous)

-- [3, 5] :: [Int]

-- [[3, 5], [5, 4]] :: [[Int]]

-- every type also has an empty list

-- [[], [[]]] :: [[[a]]]

-- x = [1..10]

(:) :: a -> [a] -> [a]

-- this is an operator and can be digven types as if they are functions of "multiple arguments"