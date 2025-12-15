module FucsAppMon() where 

-- functors are used to make something mappable, not only lists

class Functor f where 
    fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> mx = fmap f mx 

-- rules of functors
-- fmap id = id 
-- fmap f . fmap g = fmap (f.g)

--1a

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just (f x)
maybeMap _ (Nothing) = Nothing

-- -- 1b
-- maybeMap f (maybeMap g x) 
--                         = maybeMap f (Just (g x))
--                         = Just (f (g x))

-- maybeMap f (maybeMap g Nothing) = maybeMap f Nothing
--                                 = Nothing

-- maybeMap (f . g) x = Just ((f. g) x)

-- maybeMap (f. g) Nothing = Nothing 

oddMap :: (a -> b) -> Maybe a -> Maybe b 
oddMap _ _ = Nothing 

-- oddMap id (Just a) = Nothing 
-- this breaks the law if id

-- 2
-- map f (map g xs) = map f (map g (x:xs))
--                  = map f (g x : map g xs)
--                  = (f.g) x : map f (map g xs)
--                  = map (f.g) (x:xs)

-- 3
pairMap :: (a->b) -> (x, a) -> (x, b) 
pairMap f (x, y) = (x, f y)

-- pairMap id (x, y) = (x, id y) = (x, y)
-- pairMap f (pairMap g (x, y)) = pairMap (x, g y)
--                              = (x, f (g y))
--                              = (x, (f. g) y)
--                              = pairMap (f. g) (x, y)

data Treee = Leaf a | Sprout a a | Fork (Treee a) a (Treee a)

-- 4a
instance Functor Treee where 
    fmap :: (a -> b) -> Treee a -> Treee b 
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Sprout l r) = Sprout (f l) (f r)
    fmap f (Fork l x r) = Fork (fmap f l) (f x) (fmap f r)

-- applicatives 
-- class Functor f => Applicative f where 
--     -- the minimal f structure containing a value of type a 
--     pure :: a -> f a 
--     -- the app function combines 2 arguments multiplicatively 
--     (<*>)  :: f (a -> b) -> f a -> f b 

-- ie fmap f mx = pure f <*> mx 

-- instance Applicative Maybe where 
--     pure :: a -> Maybe a 
--     pure = Just 

--     (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b 
--     Nothing <*> _ = Nothing 
--     Just f <*> mx = fmap f mx 

-- instance Applicative [] where 
--     pure :: a -> [a] 
--     pure x = [x]

--     (<*>) :: [a -> b] -> [a] -> [b]
--     [] <*> _ = []
--     (f : fs) <*> xs = map f xs ++ (fs <*> xs) 

-- pure f <*> xs 
-- = [f] <*> xs 
-- = (f:[]) <*> xs 
-- = map f xs ++ ([] <*> xs) 
-- = map f xs ++ []
-- = map f xs 

-- mf <*> pure x = pure (\f -> f x) <*> mf 
-- Nothing <*> pure x = Nothing = pure (\f -> f x) <*> Nothing 

-- Just f <*> pure x = fmap f (pure x)
--                   = fmap f (Just x)
--                   = Just (f x)
--                   = fmap (\f -> f x) (Just f)
--                   = Just (\f -> f x) <*> (Just f)
--                   = pure (\f -> f x) <*> (Just f)

-- [] <*> pure x = [] = pure (\f -> f x) <*> [] 

-- (f:fs) <*> pure x = map f (pure x) ++ fs <*> pure x 
--                   = map f [x] ++ fs <*> pure x 
--                   = [f x] ++ fs <*> pure x 
--                   = f x : fs <*> pure x 
--                   = f x : map (\f -> f x) fs 
--                   = map (\f -> f x) (f:fs)
               
-- 2a
-- pure :: a -> (x, a)
-- pure x = (y, x) -- not possible, as we don't know what y is

class Semigroup s where 
    (<>) :: s -> s -> s 

class Semigroup m => Monoid m where 
    mempty :: m 
    -- mempty <> x = x = x <> mempty 
    -- (x <> y) <> z = x <> (y <> z)

instance Monoid m => Applicative ((,), m) where 
    pure :: a -> (m, a)
    pure x = (mempty, x)
    (<*>) :: (m, a -> b) -> (m, a) -> (m, b)
    (m, f) <*> (n, x) = (m <> n, f x)

-- Writer monad
-- pure (*) <*> ([1, 2, 3], 5) <*> ([4, 5, 6], 2)

sequence :: Applicative f => [f a] -> f [a]
sequence [] = pure []
sequence (mx:mxs) = (:) <$> mx <*> sequence mxs 

-- (:) <$> mx means take every element in mx and apply the cons function to it
-- the <*> sequence mxs is the app function and just recursively applies the functions onto all the items in sequences mxs

traverse :: Applicative f => [a] -> (a -> f b) -> f [b]
traverse f = sequence . map f 