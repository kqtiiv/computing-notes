> module Containers where

These imports come from the `containers` package

> import Data.Set (Set) -- Set a ~ [a]
> import Data.Map (Map) -- Map k v ~ [(k, v)]

> import Data.Set qualified as Set
> import Data.Map qualified as Map

In this lecture, we are going to discuss some practical
applications of *newtypes*, and practical (and opaque)
tree datatypes for *maps* and *sets*. We are going to
learn about abstracting and *API*s.

One of the basic ways in which we use newtypes is to
use them to keep equivalently runtime-represented data
distinct at compile-time. As an example, chess boards,
are in terms of both *ranks* and *files*, which represent
the coordinates of a piece.

> newtype Rank = Rank Int
> newtype File = File Int

By defining newtypes for these, we can ensure that they aren't
interchanged incorrectly, as follows:

< place :: Rank -> File -> Board -> Piece -> Board

However, there are other uses for newtypes. In particular,
we know that typeclasses must have unique instances for a given type.
Here is an example (you don't need to know about Monoids, however):

< class Monoid m where
<   mempty :: m
<   mappend :: m -> m -> m

This is a typeclass where `mempty` and `mappend` adhere
to some laws:

< mempty `mappend` x = x
< x `mappend` mempty = x
< mappend is associative.

We can think of several examples of a type `m`, and its `mempty`
and `mappend` adhering to these laws:

< instance Monoid [a] where
<   mempty = []
<   mappend = (++)

< instance Monoid Int where
<   mempty = 0
<   mappend = (+)

< instance Monoid Int where
<   mempty = 1
<   mappend = (*)

Ah, but now we have a problem! There are *two* `Monoid Int`s,
which is not allowed. In fact, GHC doesn't pick either of them
and there is no `Monoid Int` built in. Instead, we can use
newtypes to work around the unique instance restriction:

< newtype Sum = Sum Int
< newtype Product = Product Int

< instance Monoid Sum where
<   mempty :: Sum
<   mempty = Sum 0
<   mappend :: Sum -> Sum -> Sum
<   mappend (Sum x) (Sum y) = Sum (x + y)

We can do similar with `Monoid Product`. Both `Sum` and `Product`
(as well as `Any` and `All` which are newtypes on `Bool`) are found in
`Data.Monoid`.

Another useful way of using newtypes is to alter an existing instances
behaviours. We can sort a list backwards like so:

< reverse . sort

But is there a way we could do it in one pass? Yes!

< sortOn Down

Down is defined in `Data.Ord`

< newtype Down a = Down a deriving Eq
< instance Ord a => Ord (Down a) where
<   Down x <= Down y = x >= y

Containers
----------

Now we're going to focus on treeess.

In the weekday lectures, you met trees, and the `grow` function,
which turns a list into a tree. However, when the list is sorted,
it will produce a biased tree that only grows in one direction.
This is not ideal when we wanted to be able to do *binary search*
on those trees. Implementing trees that try and balance themselves
is the answer, however, that is far too complex for us at this
stage. Instead, we can reach for work that other people have done
for us: in Haskell, a balanced tree of ordered things with no
duplicates is called a `Set`:

< xs = Set.fromList [1, 2, 3]
< ys = Set.fromList [3, 4, 5]
< zs = Set.union xs ys -- Set.fromList [1, 2, 3, 4, 5]

These are efficient to add elements into (with `Set.insert`, note the qualified
import!) and query members from (with `Set.member`)

Here is a function I don't like:

< nub :: Eq a => [a] -> [a] -- O(n^2)

Basically, `nub` removes duplicates from a list, by using only `Eq`,
and preserves the order of the first seen element:

< nub [1, 2, 4, 3, 5, 3, 1] = [1, 2, 4, 3, 5]

When we have `Ord`ered data, however, we could do better.
Let's define `nubOrd`, which can be found Data.Containers.ListUtils

> nubOrd :: forall a. Ord a => [a] -> [a] -- O(n log n)
> nubOrd xs = nub' Set.empty xs

We are going to define it in terms of a helper function `nub'` which
carries a set through it. This will track which elements we've seen
during the traversal; we shouldn't add elements found in this set
in again.

>   where nub' :: Set a -> [a] -> [a]
>         nub' _ [] = []
>         nub' seen (x:xs)
>           | Set.member x seen = nub' seen xs
>           | otherwise         = x : nub' (Set.insert x seen) xs

This works, and is more efficient than `nub`: win!
This of course does duplicate the representation of the list twice,
but that is important, since Set does not preserve ordering, which
was a requirement of `nub`.

> nubOrdBad :: Ord a => [a] -> [a]
> nubOrdBad = Set.toList . Set.fromList

This does remove duplicates, but it will also sort the list, which we
didn't want. Sometimes, redundant representation is a *good thing*.

Sets have a cousin called Maps, which are great for key-value mappings.

< m1 = Map.fromList [("x", 0), ("y", 1)]

As opposed to using `fromJust . lookup`, or `head . map snd . filter ((== k) . fst)`,
we can use `Map.!`. For an equivalent to `lookup`, which returns a `Maybe`,
we also have `Map.lookup`. `Map.!` is much more efficient than `(!!)`, and
should not be feared.

As an example of maps, here is a function I really wish that Haskell had:

< groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]

If you've met `group`, you'll know it works for `Eq` and bundles *adjacently*
equal elements into sub-lists. To use it for anything useful, you normally
need to use `group . sort`; this is a little annoying. Sometimes you want
to generically partition a list into multiple different groupings based on
some property they have in common. That's what `groupBy` (which is not in base
Haskell) is for. Let's see an example of what we want it to do

> data Fruit = Apple | Pear | Banana | Lemon | Mandarin | Blueberry deriving (Show)
> data Colour = Green | Yellow | Orange | Blue | Red deriving (Eq, Ord, Show)

> fruits :: [Fruit]
> fruits = [Apple, Pear, Apple, Lemon, Lemon, Mandarin, Banana]

> colour :: Fruit -> Colour
> colour Apple = Green
> colour Pear = Green
> colour Lemon = Yellow
> colour Banana = Yellow
> colour Mandarin = Orange
> colour Blueberry = Blue

< byColour :: Map Colour [Fruit]
< byColour = groupBy colour fruits
<          = Map.fromList [ (Green,[Apple,Pear,Apple])
<                         , (Yellow,[Lemon,Lemon,Banana])
<                         , (Orange,[Mandarin])
<                         ]

Nice! This function, in practice, is super useful. Lets have *three* goes
at defining it. If we look into Data.Map, we'll learn about three
functions: `Map.member`, `Map.insert`, and `Map.!`:

< groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
< groupBy f [] = Map.empty
< groupBy f (x:xs)
<   | Map.member k m = Map.insert k (x : (m ! k)) m
<   | otherwise      = Map.insert k [x] m
<   where k = f x
<         m = groupBy f xs

This works, and it's saying that if the element is in the map already,
get the list out, add `x` to the front, then put it back. Otherwise,
insert the singleon list. This seems a bit messy, so we can go and hunt
around for another function to help with the first bit... After a
search, we might find `Map.adjust`, which updates a mapped value with
a given function:

< groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
< groupBy f [] = Map.empty
< groupBy f (x:xs)
<   | Map.member k m = Map.adjust (x :) k m
<   | otherwise      = Map.insert k [x] m
<   where k = f x
<         m = groupBy f xs

That's a bit better! However, we can do better still with a more thorough
search. We could find the function `Map.insertWith`, which promises to
insert a value if it isn't already there, and merge it with an existing one
using a given merging function if it was already.

> groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
> groupBy f [] = Map.empty
> groupBy f (x:xs) = Map.insertWith (++) k [x] (groupBy f xs)
>   where k = f x

Nice!

Just for completeness, we could also do it as a fold:

> groupBy f = foldr (\x -> Map.insertWith (++) (f x) [x]) Map.empty