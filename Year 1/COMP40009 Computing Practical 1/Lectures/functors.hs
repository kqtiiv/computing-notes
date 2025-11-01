In older versions of GHC, deriving functor is not something we
can do by default. Instead, we needed to turn on a language
extension for it. This is how you would have done that:

< {-# LANGUAGE DeriveFunctor #-}

This lecture we will learn about how to generalise our `map` functions.
We will also start working our way towards *the end*.

> module Functors where

This is where the course gets harder again. Take the time you need to
digest this stuff.

> import Prelude hiding (map)

< data [] a = [] | a : [a]

> data Tree a = Tip | Node (Tree a) a (Tree a) deriving (Show)

Two weeks ago, we met the `map` function on lists:

> map :: (a -> b) -> [a] -> [b]
> map f [] = []
> map f (x:xs) = f x : map f xs

Last lecture we also saw how to map `Tree`:

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f Tip = Tip
> mapTree f (Node lt x rt) = Node (mapTree f lt) (f x) (mapTree f rt)

Let's also do this for `Bush`:

> data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving (Show, Functor) -- spoilers!

> mapBush :: (a -> b) -> Bush a -> Bush b
> mapBush f (Leaf x) = Leaf (f x)
> mapBush f (Fork lt rt) = Fork (mapBush f lt) (mapBush f rt)

And we can also do the same for rose-bushes:

> data RoseBush a = Flower a | Vine [RoseBush a] deriving (Show, Functor) -- spoilers!

> mapRose :: (a -> b) -> RoseBush a -> RoseBush b
> mapRose f (Flower x) = Flower (f x)
> mapRose f (Vine ts) = Vine (map (mapRose f) ts)

We've written three functions which all do the same sort of thing:

* They each turn `a`s into `b`s, wherever they are found
* They each preserve *perfectly* the surrounding structure

The shapes of these functions are all similar, but the
concrete type varies. We might imagine making them
more polymorphic, with a typeclass to allow implementation
to vary:

< ? :: ? t => (a -> b) -> t a -> t b

This is strange! `t` is a polymorphic type variable taking arguments!

Kinds
-----
All the values we've seen so far have had types.

< True :: Bool
< 'a' :: Char
< [] :: [a]
< Nothing :: Maybe a
< Just :: a -> Maybe a

It turns out that there is a level higher, called
*kinds*:

< Bool :: *
< Char :: *
< Maybe a :: *
< [a] :: *

All things that have values have their types as kind *.

However, we can also have type constructors:

< Maybe :: * -> *
< [] :: * -> *
< (->) :: * -> * -> *
< Tree :: * -> *
< Bush :: * -> *
< RoseBush :: * -> *

< data Either a b = Left a | Right b

< Either :: * -> * -> *

So, in our proposed type above:

< ? :: ? t => (a -> b) -> t a -> t b

`t` would have kind `* -> *`, since it is applied to an `a`, and `t a`
must be of kind `*` to be passed into a function.

So, there is a typeclass that abstracts all of these different map
functions. It has an obtuse name that we just need to remember.

< class Functor (t :: * -> *) where
<   fmap :: (a -> b) -> t a -> t b

Functors have a method called `fmap`, which stands for "functor map".
Let's see an example:

> instance Functor Tree where
>   fmap :: (a -> b) -> Tree a -> Tree b
>   fmap f Tip = Tip
>   fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)

This is the same definition that we had for `treeMap` above!
By giving it the more general name of `fmap` we can bundle
it together with other structures that also support mapping.
Why is this a good thing? Well, it allows us to generalise
different functions.

> mapSquare :: (Functor t, Num a) => t a -> t a
> mapSquare = fmap (\x -> x * x)

This function maps all the `a`s, which are `Num`bers
and squares them all, leaving the remaining shape of the
structure untouched.

It turns out, writing Functor instances are reasonably mechanical:
Always apply `f` to `a`s, wherever you see them, then use `fmap`
to handle recursive structures, then if you see a recursive site
inside a structure that is also a functor, `fmap` the `fmap`, and
so on. As you would have had spoiled for you above, GHC can `derive Functor`
and do this work for us *most of the time*.

We might ask, what things are functors, and what things *aren't*.
One example of something suprising that is, is `(x ->)`. First,
let's get comfortable with the fact that `(x ->)` is legal:

(+) :: Int -> Int -> Int
(1 +) :: Int -> Int
(+ 1) :: Int -> Int

These 3 things above are similar, in principle, to the three things below:

(->) :: * -> * -> *
(Int ->) :: * -> *
(-> Int) :: * -> *

In other words, we can partially apply types, and (->) is an operator,
which we can partially apply on either side of. Things of the form
`(x ->)` are functors, which allows the return type to vary:

< instance Functor (x ->) where
<   fmap :: (a -> b) -> (x -> a) -> (x -> b)
<   fmap = (.)

This is exciting! Partially applied types, can still be functors.
Does that work the other way around:

< instance Functor (-> x) where
<   fmap :: (a -> b) -> (a -> x) -> (b -> x)
<   fmap = DOESN'T EXIST

This tells us something about the nature of functors:
they work for *producers* of a type `a`, but not *consumers*.

We can come up with some laws, that help us guarantee the behaviours.

< fmap id = id

This helps us guarantee that `fmap` cannot change the *shape* of the
data. If we do nothing to the elements, then nothing happens.

< fmap f . fmap g = fmap (f . g)

This law also reinforces the above point, *and* it is an optimisation.
It is important that we adhere to the laws, but GHC won't check for
us. BE CAREFUL.

Let's go through the motions, and see that `(x ->)` adheres to these laws:

First we need to show that `fmap id f = f`:

< fmap id f = (.) id f -- by definition of fmap
<           = id . f   -- switch to familiar infix notation
<           = f        -- composing id always does nothing

Now we need to show that `fmap f (fmap g h) = fmap (f . g) h`

< fmap f (fmap g h) = (.) f ((.) g h) -- by definition of fmap
<                   = f . (g . h)     -- switch to familiar infix notation
<                   = (f . g) . h     -- by associativity of (.)
<                   = (.) (f . g) h   -- switch to prefix
<                   = fmap (f . g) h  -- definition of fmap

Excellent! We've now proved that `x ->` is a legal functor.

Let's see two new-ish datatypes:

< data Maybe a = Nothing | Just a
< data Either a b = Left a | Right b

Both of these are functors:

< instance Functor Maybe where
<   fmap :: (a -> b) -> Maybe a -> Maybe b
<   fmap f Nothing = Nothing
<   fmap f (Just x) = Just (f x)

Let's look at `Either`. In essence, if you're a functor,
you are a functor in the *last* type parameter. In either's
case, that's `b`.

< instance Functor (Either x) where
<   fmap :: (a -> b) -> Either x a -> Either x b
<   fmap f (Left x) = Left x
<   fmap f (Right y) = Right (f y)

For `Either`, `fmap` only works for `Right` values.

> data Triple a b c = Fst a | Snd c | Thd b

> instance Functor (Triple x y) where
>   fmap :: (a -> b) -> Triple x y a -> Triple x y b
>   fmap f (Fst x) = Fst x
>   fmap f (Snd y) = Snd (f y)
>   fmap f (Thd z) = Thd z

Here we again see that the `c` is the only thing we can map in `Triple a b c`,
it so happens you can find a `c` inside the `Snd` constructor. As I said earlier,
this is pretty mechanical, and we could have used `deriving Functor` instead.

Why are `Maybe` and `Either` useful?
The role they play is about "failure". With `Either` `Left`s are errors,
and `Right`s are successes. With `Maybe`, `Nothing` is a failure, and `Just`
is a success.

Let's see an example:

> headMaybe :: [a] -> Maybe a
> headMaybe [] = Nothing
> headMaybe (x:_) = Just x

> safeDiv :: Int -> Int -> Maybe Int
> safeDiv _ 0 = Nothing
> safeDiv m n = Just (div m n)

Both of these functions are taking an error case, which would normally
crash (see `head []` and `div 5 0` in GHCi), and is instead explicitly outlining
that this error would be represented by `Nothing`, the absence of value.

With `fmap`, we can play around with these functions and do more things to
the results by "assuming success":

< fmap (+1) (safeDiv 5 6) = Just 2
< fmap (\x -> x * x) (safeDiv 10 5) = Just 4
< fmap (+4) (safeDiv 5 0) = Nothing

When we use `fmap` we get to "peek" underneath the `Maybe` in our
function and deal with only successful cases. This is nice, but is
actually quite limited in power. Let's see an example where we quickly
get stuck:

Let's say I have:

> mx, my, mz :: Maybe Int
> mx = safeDiv 5 2
> my = safeDiv 8 3

Can I "add" `mx` and `my` together (to get a `Maybe Int`)?
At the moment we are a bit stuck -- we only know, with fmap,
how to work on *one* Maybe at a time, two is too many!
Remember `fmap` preserves structure, so there is no way
it can *combine* structure to make new structure. Instead,
we can roll our own:

> addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
> addMaybe Nothing _ = Nothing
> addMaybe _ Nothing = Nothing
> addMaybe (Just x) (Just y) = Just (x + y)

This function works, but only for `Maybe`, and
not for `[]`, or `Either a`, etc etc. This is a shame.

There are two sensible ways of implementing something that
looks similar on lists:

> addListZip :: [Int] -> [Int] -> [Int]
> addListZip = zipWith (+)

> addListCart :: [Int] -> [Int] -> [Int]
> --addListCard xs ys = [x + y | x <- xs, y <- ys]
> addListCart [] _ = []
> addListCart (x:xs) (y:ys) = map (x +) ys ++ addListCart xs ys

Which one do we want, exactly? For sane of divine intervention,
we'll pick a "cartesian"-style one. The zippy version is also
a good choice, but doesn't generalise nearly as well -- we'll
see it on Friday instead.

As we will see tomorrow, `addListCart` and `addMaybe` can generalise
together into something a bit more powerful than `Functor`...