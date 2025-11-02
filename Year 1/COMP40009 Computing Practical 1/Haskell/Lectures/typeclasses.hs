This lecture is about Ad-hoc Polymorphism, A.K.A Typeclasses.

> module Typeclasses where

> import Prelude hiding (elem)

> import Data.List (intercalate)

When we are programming, sometimes we want to give the same name for
something that is well-defined on more than one thing. As an example,
it would be tedious if we had to make unique names for every single
type's `==` function:

< intEqual :: Int -> Int -> Bool
< boolEqual :: Bool -> Bool -> Bool
< charEqual :: Char -> Char -> Bool

When we have types that include polymorphism, then we get even more
stuck:

< listEqual :: (a -> a -> Bool) -> [a] -> [a] -> Bool

We would need to say how to compare `a`s for equality as an argument
to this function.

Safe to say, this is a mess, and would make programming very inelegant.
Why can't we just have `(==)` work on everything?

< equal :: a -> a -> Bool

The problem with this parametrically polymorphic function is that
we now know *absolutely* nothing about `a`. The types allow me,
say, to call `equal (+1) (-1)`, this is well typed, as `a ~ (Int -> Int)`,
but there is no meaningful definition for equality of functions.
To make this work properly, we turn instead to *ad-hoc* polymorphism,
where we can be more specific about the types we allow, and allow their
implementations to vary. We saw an example of this in one of the Friday
lectures, where we learnt that numbers are represented by something
called `Num`:

         v--- is called a "constraint"
< 7 :: Num a => a
       ^ "Num" is called a typeclass

                             v  everything before this are the constraints
< 7 :: (Show a, Eq a, Num a) => a
                                ^ normal function types

You can write the below, but for some reason, we don't:

< 7 :: Show a => Eq a => Num a => a

It is possible to say `7 :: Int`, `7 :: Double`, `7 :: Integer`. These
are all possible because the compiler can find an *instance* of `Num Int`,
`Num Double` or `Num Integer`.

Polymorphism is good because it makes our functions
more reusable. We've seen parametric polymoprhism already:
this is just `a`s and `b`s and so on. We can't assume anything
about them. When we use *typeclasses* and
"ad-hoc polymorphism" we can assume *some* things. For instance:

> elem :: Eq a => a -> [a] -> Bool
> elem x [] = False
> elem x (y:ys) = x == y || elem x ys

The `elem` function looks through a list to find if a value can be
found inside it. We need to ask "is x == y", and as a result, we
cannot have this function for FOR ALL `a`s, it can now only work
for `a`'s which are known to be comparable for equality, in other
words: types `a` for which there is an `Eq a` instance available.

Let's understand what `Eq` looks like:

< class Eq a where
<   (==) :: a -> a -> Bool -- this is called a "method"

The above method *must* be defined when we want a type
to be Eq. However, methods can be given default
implementations sometimes.

<   (/=) :: a -> a -> Bool
<   x /= y = not (x == y)

We don't have to define these in our instances, but we *could*.

Let's say we have a type:

< data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun

We could certainly compare these days for equality:

< instance Eq Day where
<   (==) :: Day -> Day -> Bool
<   Mon == Mon = True
<   Tues == Tues = True
<   Wed == Wed = True
<   Thurs == Thurs = True
<   Fri == Fri = True
<   Sat == Sat = True
<   Sun == Sun = True
<   -- x == x = True -- this is tempting, but does not work
<   _ == _ = False

This was boring, but is correct. Can we do better?

Yes!

> data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Eq, Ord, Show)

`deriving Eq` tells the compiler do to the boring stuff (spoilers for
later, we can also derive `Ord` and `Show`, which we'll also see in this
lecture).

Let's see a more complex instance (with some derived instances):

> data Nat = Z | S Nat deriving (Eq, Ord, Show)

This describes all natural numbers in a clumsy way (`S` is +1 and `Z` is 0)

> one = S Z
> two = S (S Z)

While I have derived equality above, it is instructive
to see what the instance might look like:

< instance Eq Nat where
<   (==) :: Nat -> Nat -> Bool

The base case for this function is straightforward:

<   Z   == Z   = True

Otherwise we need to think about the recursive case.
We have two cases, if the numbers are `Z` and `S` in
either way round, they are obviously not equal. Otherwise,
we know that both must be `S`. You can read this case
as `m + 1 == n + 1`, these are equal when `m == n`:

<   S m == S n = m == n
<   _   == _   = False

So, we've seen a recursive equality function here, and GHC
can still derive it for us, clever GHC :)

Ok, what about with polymorphic structures. To
compare two lists for equality, they must both
be the same length, and all their elements must
pairwise be equal. This means for a `Eq [a]` instance,
we also need to know that `Eq a` is available to
compare the elements. We express this in the instance "head":

< instance Eq a => Eq [a] where
<   (==) :: [a] -> [a] -> Bool

Much like with the natural numbers above, we need to
structurally inspect for similar things. The difference
is we also check that for two non-empty lists, their head
elements are equal:

<   []     == []     = True
<   (x:xs) == (y:ys) = x == y && xs == ys
<   _      == _      = False

In a similar vein, we can compare tuples for equality,
so long as we know how to compare all their elements
for equality:

< instance (Eq a, Eq b) => Eq (a, b) where
<   (x1, y1) == (x2, y2) = x1 == x2 && y1 == y2

As before, GHC knows how to handle this too in the derivation,
clever GHC :)

One final note about `Eq`. Every typeclass realistically
comes equipped with laws, which the class author wants us
to adhere to in our instances (though this is not "part"
of the language). For instance, for `Eq`, we expect it
to be true that:

< x == y = y == x
< x == y && y == z = x == z

While these laws are not checked by the compiler, you violate
them at your peril -- things will get weird and you won't
understand why. Stick to the laws and you will not get tripped up.

Once we can compare for equality, can we also do orderings?
Yes! For something to be compared for ordering, it must
already be comparable for equality. Technically, if we
know we have `(==)` already, we only need a definition for `(<=)`
and we can get all the other comparison operators "for free":

< class Eq a => Ord a where
<   (<=) :: a -> a -> Bool
<
<   (<) :: a -> a -> Bool
<   x < y = x <= y && x /= y
<
<   (>) :: a -> a -> Bool
<   (>) = flip (<)
<   (>=) :: a -> a -> Bool
<   (>=) = flip (<=)

One final interesting method for `Ord` is the function
`compare`, which returns an `Ordering`:

<   compare :: a -> a -> Ordering
<   compare x y
<    | x < y     = LT
<    | x == y    = EQ
<    | otherwise = GT

This can be given a more efficient implementation, so could
be cheaper than otherwise doing two comparison checks.
We will see examples of this in use in tomorrow's lecture.

< data Ordering = LT | GT | EQ

This is also very boring to do by hand:

< instance Ord Day where
<   Mon <= _ = True
<   Tues <= Mon = False
<   Tues <= _ = True
<   ...

We can also derive `Ord` and that saves time. But let's define
for lists:

< instance Ord a => Ord [a] where
<   [] <= _ = True
<   _ <= [] = False
<   (x:xs) <= (y:ys)
<     | x < y  = True
<     | x == y = xs <= ys
<     | otherwise = False

This is the so-called dictionary ordering, it works the
same way that words are ordered in the dictionary. If
all the letters in the words are the same but one is shorter,
the shorter one comes first: `"app" < "apple"`. Otherwise,
compare eaach pair of letters -- if they are the same, throw
them away and compare the rest of the list, otherwise as soon
as one is less than the other, we are done. `"pad" < "pea"`.

To see the `compare` function from above, I could have also
written:

<   (x:xs) <= (y:ys) = case compare x y of
<     LT -> True
<     EQ -> xs <= ys
<     GT -> False

Now that we have orderings, we can write a function that
uses this. The `insert x xs` function will insert the
element `x` into ordered list `xs` such that the ordering
is preserved.

> -- pre: the input list is ordered
> insert :: Ord a => a -> [a] -> [a]
> insert x [] = [x]                  -- nothing to do here, `x` belongs at the end
> insert x yys@(y:ys)
>   | x <= y    = x : yys            -- if `x` is less than the head, it goes at the front
>   | otherwise = y : insert x ys    -- otherwise, we need to push it further into the tail

We can use this to sort a list, which is called "insertion sort":

> isort :: Ord a => [a] -> [a]
> --isort [] = []
> --isort (x:xs) = insert x (isort xs)
> isort = foldr insert []

This is not the best sorting algorithm in the world, but that's for next year!

Making Strings
--------------
When we try and use values in the terminal, GHCi sometimes gives an error like:

<interactive>:9:1: error:
    • No instance for (Show Something)
        arising from a use of `print`

GHCi needs a `Show` instance to print things:

< class Show a where
<   show :: a -> String

The `Show` typeclass is also derivable:

< show (S (S (S (Z)))) = "S (S (S Z))"

It minimises brackets :), which makes it otherwise
quite hard to write by hand.

There is a law for `Show` relating it to `Read`:

< class Read a where
<   read :: String -> a

(which is also derivable).

< read . show = id

This is hard to adhere to, so we should *always* derive Show and Read.
The compiler knows what its doing, and we certainly do not. Don't be
tempted to write it yourself!

If we want a typeclass for *pretty printing*, we should make our own:

> class Pretty a where
>   pretty :: a -> String

For instance, we can pretty print natural numbers by turning them
into a number and turning that into a string instead:

> instance Pretty Nat where
>   pretty n = show (toInteger n)
>     where toInteger Z = 0
>           toInteger (S n) = toInteger n + 1

< pretty (S (S Z)) = "2"

We can write a `Pretty` instance for lists too:

> instance Pretty a => Pretty [a] where
>   pretty xs = "[" ++ intercalate "," (map pretty xs) ++ "]"

The `intercalate` function is great fun; it will take
each of the elements of `map pretty xs` and put the ","
between them all, then flatten the list back down. Do check
out Data.List, and see what goodies are in it. You might
find something you wish you'd had the whole time!

Making Numbers
--------------

< data Nat = Z | S Nat deriving (Eq, Ord, Show)

Here is finally a typeclass we cannot derive (*gasp*).
We have the typeclass for "number-like things": `Num`.

< class Ord a => Num a where
<   fromInteger :: Integer -> a
<   (+) :: a -> a -> a
<   (-) :: a -> a -> a
<   (*) :: a -> a -> a
<
<   abs :: a -> a
<   signum :: a -> a

The numeric hierarchy in Haskell is a bit of a mess;
there is also `Fractional`, `Integral`, and `Floating`
as well with different additional operators on top of
`Num`. You can always use `:i Integral`, say, in GHCi
to learn more about a typeclass (including who has instances
for it already). Let's see if we can describe how to use
our "Peano" natural numbers as "normal" numbers:

> instance Num Nat where

First we need to describe how we can go from an integer
into the type `Nat`. This is what the compiler secretly
uses whenever we write a literal. For instance, `123`
will elaborate to `fromInteger 123` behind the scenes.
For this, we will write an inductive function on integers,
taking care to handle negatives:

>   fromInteger :: Integer -> Nat
>   fromInteger 0 = Z
>   fromInteger n
>    | n > 0     = S (fromInteger (n - 1))
>    | otherwise = undefined

Now we need to define addition on our Peano naturals. We
also do this by induction on the structure of the numbers.
The "trivial" case here is when we compute `0 + n`, which is,
by the laws of `Num`, just `n`. In the recursive case, we
just need to remember to add one to the recursion.

>   (+) :: Nat -> Nat -> Nat
>   Z + n     = n             -- 0 + n = n; by laws of arithmetic
>   (S m) + n = S (m + n)     -- (m + 1) + n = (1 + m) + n = 1 + (m + n); also by laws of arithmetic

Multiplication is a bit more tricky, but we can also use
the laws of natural numbers to formulate this in a way
that is easier to reason about. Start with out base case,
`0 * n = 0` by the laws of arithmetic (specifically "rings").
Otherwise, we have `(1 + m) * n`. Let's simplify this:

< (1 + m) * n
<   = -- by right-distributivity of (+) and (*)
< (1 * n) + (m * n)
<   = -- by left-unit of multiplication
< n + (m * n)

Now this is something we know how to do straightforwardly!

>   (*) :: Nat -> Nat -> Nat
>   Z * n     = Z
>   (S m) * n = n + (m * n)

For subtraction, we need to be a bit more careful, the laws
tell us that `m - 0 = m`, so that makes a good base case.
If we subtract anything from 0 that is undefined. Otherwise,
we know we have something of the form `(m + 1) - (n + 1)`;
we can simplify this straightforwardly to `n - m`.

>   (-) :: Nat -> Nat -> Nat
>   m - Z = m
>   Z - _ = undefined
>   (S m) - (S n) = m - n

Now we finally need a definition for absolute numbers (this
is easy, naturals are always positive!) and the signum of
a natural (which is its parity):

>   abs :: Nat -> Nat
>   abs = id
>
>   signum :: Nat -> Nat
>   -- the 0 and the 1 on the right-hand side are going via `fromInteger`
>   signum 0 = 0
>   signum _ = 1

Now we can have a play around:

< 5 + 3 :: Nat
<   = S (S (S (S (S (S (S (S Z)))))))
< 4 * 3 :: Nat
<   = S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))

Lovely.