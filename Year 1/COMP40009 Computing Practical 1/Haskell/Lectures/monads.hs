In this lecture, we should be able to print "Hello world"

> module Monads where

> import Control.Applicative
> import Control.Monad

So far in the story, we have seen two levels of abstraction:

* `Functor`s, which allow us to generalise mapping to work over
  many structures
* `Applicative`s, which allow us to combine two structures in a
  multiplicative way, but only where the function we use cannot
  itself introduce new structure.

Applicative were useful, we saw examples last week:

> addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
> addMaybe = liftA2 (+)

This is a function that given two maybes, will add their
values when they are both `Just`, and return `Nothing`
otherwise.

> safeDiv :: Int -> Int -> Maybe Int
> safeDiv m 0 = Nothing
> safeDiv m n = Just (div m n)

This is a function that will return Nothing if we try and do a
division-by-zero, otherwise performs the division.

The first question is can we represent something like:

< (10 `div` 2) `div` (6 `div` 2)

But where we safely account for the possibility of division
by zero? Let's try using `liftA2 safeDiv` and see why that
doesn't produce the most sensible results:

< liftA2 safeDiv (safeDiv 10 2) (safeDiv 6 2) = Just (Just 1)
< liftA2 safeDiv (safeDiv 10 0) (safeDiv 6 2) = Nothing
< liftA2 safeDiv (safeDiv 10 2) (pure 0)      = Just Nothing

These results are plain confusing. They don't match our
intuition for what this should do. The problem is that
`safeDiv` itself makes Maybes, so the applicative creates
a nested structure. What we want is `Just 1` or `Nothing`.
Fixing this is the role of Monads.

Monads
------
Like Applicatives can be implemented by *either* liftA2 or `(<*>)`,
there are two ways of defining `Monad`s. One is called `join`, the
other is called `(>>=)` ("bind"). Let's look at `join` first:

< join :: Monad m => m (m a) -> m a

What does `join` do? It is able to flatten structures.

> joinMaybe :: Maybe (Maybe a) -> Maybe a
> joinMaybe Nothing = Nothing
> joinMaybe (Just mx) = mx

If we see a `Nothing`, there is no `Maybe a` to fish out
from inside of it. As such, we still produce `Nothing`.
Otherwise, if we have a `Just` then it contains a `Maybe a`,
and we can just return that thing straight away.
Let's look at the equivalent function, but on lists:

< joinList :: [[a]] -> [a]
< joinList [] = []
< joinList (xs:xss) = xs ++ joinList xss

Ok, so if we have an empty list, there is nothing to produce,
similar to the Nothing. Otherwise, we take a list out and
add it onto the flattening of everything else. In other words:

< joinList = concat

This is interesting! Right, now we have `joinMaybe`, we can use
it to try and fix our problem with the `Maybe (Maybe Int)` from
before:

< joinMaybe (liftA2 safeDiv (safeDiv 10 2) (safeDiv 6 2)) = Just 1
< joinMaybe (liftA2 safeDiv (safeDiv 10 2) (pure 0)) = Nothing
< joinMaybe (liftA2 safeDiv (safeDiv 10 0) (pure 5)) = Nothing

This gives us sensible results, but it's a bit clumsy.

`joinList` is `concat` in disguise. In fact, it's better than this,
the `join` function generalises `concat` for all Monadic structures.
So I also said that we have `(>>=)`, pronounced "bind":

< (>>=) :: Monad m => m a -> (a -> m b) -> m b

What's the intuition we should take from this? If `(<*>)`
combines two things, `>>=` consumes one thing to produce
another. If we have an `m a` we can extract all the `a`s
found inside and use them to make `m b`s, and do these.
Let's see two examples:

> bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
> bindMaybe Nothing f = Nothing
> bindMaybe (Just x) f = f x

In the `Nothing` case, there is no `a` we have to apply
our function `f` to: necessarily we return `Nothing`. In
the `Just` case, we have an `a`, so we can call `f x` to
get another `Maybe b` back. In other words, if `Maybe a`
was successful, keep going with `f` next.

> bindList :: [a] -> (a -> [b]) -> [b]
> bindList [] f = []
> bindList (x:xs) f = f x ++ bindList xs f

If we look very carefully at this, we can spot it
is the definition of `concatMap`, just flipped.
Just like `join` was the generalisation of `concat`,
`(>>=)` is the generalisation of `concatMap` for any
monadic structure. Just like we can define `concatMap`
in terms of `map` and `concat`, so to can we define
`(>>=)`:

< (>>=) :: Monad m => m a -> (a -> m b) -> m b
< mx >>= f = join (fmap f mx)

And, just like we can say `concat = concatMap id`, we
can write:

< join :: Monad m => m (m a) -> m a
< join mmx = mmx >>= id

And indeed, just as we can say `map f = concatMap (x -> [f x])`...

< liftM :: Monad m => (a -> b) -> m a -> m b
< liftM f mx = mx >>= (\x -> pure (f x))

Amazing! Ok, so we've seen the actors, let's define our
typeclass. While we might expect both `(>>=)` and `join`
to appear inside, it turns out, because of technical reasons,
that `join` is not part of the typeclass. Instead it is
defined as above.

< class Applicative m => Monad m where
<    return :: a -> m a -- A.K.A pure
<    (>>=) :: m a -> (a -> m b) -> m b

What's this `return` thing?! Well, it's a historical accident.
Monads were invented in the 90s, but applicative didn't come
around until 2008. When monads first appeared, they had `return`
in the typeclass to "make it look more imperative" (though I maintain
this is a mistake, and a huge source of confusion). In 2018ish,
Applicative became the superclass of monad, but they left
`return` alone. Now, we are left writing `return = pure` in
every since monad instance until they finally get around to
removing it.

Ok, we have the class now, we can start writing instances:

< instance Monad Maybe where
<   return = pure
<   (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
<   Nothing >>= f = Nothing
<   Just x >>= f = f x
<
< instance Monad [] where
<   return = pure
<   (>>=) :: [a] -> (a -> [b]) -> [b]
<   xs >>= f = concatMap f xs

These definitions follow from the previous ones, just with their
correct names.

We saw earlier that `join` can help us fix our safeDiv problem.
Can `(>>=)` do any better?
It *can* but beauty is in the eye of the beholder.

< join (liftA2 safeDiv (safeDiv 10 2) (safeDiv 6 3))
< =
< safeDiv 10 2 >>= (\m ->
<   safeDiv 6 3 >>= (\n ->
<     safeDiv m n
<   )
< )

This looks really different from what we had originally.
We have to think very carefully about the order we want
to do things in. But this is quite hard to read. Let's
break it down a bit: each `>>=` is saying "do the thing
on the left, then if it was successful, do the thing
on the right, with the result available to you". So first
we `safeDiv 10 2`, if it succeeds, take the result `m` and
do `safeDiv 6 3`. If that succeeded too, we have its result `n`
(along with `m` from before), and we can finally do `safeDiv m n`.

Is there a better way of representing this logic in a more readable
way? With lists, we saw there was a nice way of writing
bunches of `concatMap` and `map`, called a list comprehension:

< [x + y | x <- xs, y <- ys] = concatMap (\x -> map (x +) ys) xs
<                            -- or, as we saw last week:
<                            = liftA2 (+) xs ys

Perhaps there is something similar for monads? Before we think
about that though, notice that `liftA2 (+)` also captured this
list comprehension... This is giving us a general recipe for
defining `liftA2` in terms of the monadic operations!

< liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
< liftM2 f mx my = mx >>= (\x -> fmap (f x) my)
<
< ap :: Monad m => m (a -> b) -> m a -> m b

This is nice, we know now that we get Applicative "for free"
with monads, this'll save us some work later. But we still would
like a nicer syntax. We have list-comprehensions, they are
equivalent to concatMaps and maps, and for monads, we have
*do notation*:

< safeDiv 10 2 >>= (\m ->
<   safeDiv 6 3 >>= (\n ->
<     safeDiv m n
<   )
< )
< = -- becomes
< do m <- safeDiv 10 2
<    n <- safeDiv 6 3
<    safeDiv m n

This time, every `<-` represents a `>>=`, in the same way
every `<-` in a list comprehension represented a `concatMap`.
Unlike list-comprehensions, however, this syntax does not have
to end with a `map`. We can see the last thing in that above
`do`-block is to call `safeDiv m n`.

Let's take `liftM2` and re-write it with do-notation:

< -- liftM2 f mx my = mx >>= (\x -> fmap (f x) my)
< liftM2 f mx my = do
<   x <- mx
<   y <- my
<   pure (f x y)

< ap :: Monad m => m (a -> b) -> m a -> m b
< ap mf mx = do
<   f <- mf
<   x <- mx
<   pure (f x)

In general, whenever you see a bunch of lines in a `do`-block,
and none of the results are used until the last line, *and* that
line is a `pure`/`fmap` then this could have just been written
applicatively. I.e. applicatives can be used when the things you
are performing in sequence do not depend on each other's *results*.

One last monad example. Last week, I said that `Bush`

> data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving (Show, Functor)

Was an `Applicative`, but it was too hard to understand.
In fact, `Bush` is also a monad, and it's easier to understand
the `>>=`/`join`. Later, we can work backwards and figure out
what `liftA2` would do on a tree.

> instance Applicative Bush where
>   pure :: a -> Bush a
>   pure x = Leaf x

Before, we got as far as spotting that the minimal way to
make a bush of size 1 is to use `Leaf`. Now that we have
the `ap` function, we can use it to "skip" the rest of the
applicative definition:

>   (<*>) :: Bush (a -> b) -> Bush a -> Bush b
>   (<*>) = ap

Now it suffices to define `Monad Bush`.

> instance Monad Bush where
>   return = pure

Before we do `(>>=)`, let's do `join` to get some intuition
for how this should work:

< joinBush :: Bush (Bush a) -> Bush a

The signature is giving us a hint: we have bushes where at
their leaves, they contain other bushes. The idea is we
want to "graft" them together to make a complete, bigger bush.

Let's say we have:

< t = Fork (Leaf (Fork (Leaf 1) (Leaf 2))) (Leaf (Leaf 3))

Where each leaf contains another bush, we would want `joinBush t`
to be something like

< joinBush t = Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)

In other words, remove the `Leaf` wrapped around the sub-trees
and just plop them straight back in the hole:

< joinBush (Leaf t) = t

With the `Fork` case, we actually just leave them unchanged,
other than the fact their children are recursively flattened:

< joinBush (Fork lt rt) = Fork (joinBush lt) (joinBush rt)

Ok, so now let's take it one step further and think about
the `(>>=)`. Let's think about the type more carefully:

>   (>>=) :: Bush a -> (a -> Bush b) -> Bush b

This time, we have a bush with `a`s at the leaves. We have
a function that can turn `a`s into new bushes with `b`s at
the leaves. In the end, we want to build a single bush with
`b`s. This is very similar to flattening, however, this time
we use `f` to form the sub-tree:

>   Leaf x >>= f = f x

In the `Fork` case, it's just the same as before:

>   Fork lt rt >>= f = Fork (lt >>= f) (rt >>= f)

Let's see what this function does, then. While we are
in the throes of Autumn right now, and the trees are
sleeping; let's look forward to warmer times, and
what they might get up to then. The function `sprout`
is a function that grows a small tree:

> sprout :: Int -> Bush Int
> sprout n = Fork (Leaf n) (Leaf (n + 1))

When given an `n`, it makes a small tree with `n` and `n+1`
inside. In the Spring, when the trees grow back their
leaves, each of them will sprout forth:

> spring :: Bush Int -> Bush Int
> spring t = t >>= sprout

The function `spring` will replace every leaf in the bush `t`
with a small sprout formed from the value in that leaf.
Let's see an example:

> t :: Bush Int
> t = Fork (Fork (Leaf 1)
>                (Leaf 3))
>          (Fork (Leaf 5)
>                (Leaf 7))

When Spring comes, what will this tree become?

< spring t =
<   Fork (Fork (Fork (Leaf 1)
<                    (Leaf 2))
<              (Fork (Leaf 3)
<                    (Leaf 4)))
<        (Fork (Fork (Leaf 5)
<                    (Leaf 6))
<              (Fork (Leaf 7)
<                    (Leaf 8)))

What a majestic tree. We could even grow it bigger
if we wanted:

< fmap (*2) (spring t) =
<   Fork (Fork (Fork (Leaf 2)
<                    (Leaf 4))
<              (Fork (Leaf 6)
<                    (Leaf 8)))
<        (Fork (Fork (Leaf 10)
<                    (Leaf 12))
<              (Fork (Leaf 14)
<                    (Leaf 16)))

< spring (fmap (*2) (spring t)) =
<   Fork (Fork (Fork (Fork (Leaf 2)
<                          (Leaf 3))
<                    (Fork (Leaf 4)
<                          (Leaf 5)))
<              (Fork (Fork (Leaf 6)
<                          (Leaf 7))
<                    (Fork (Leaf 8)
<                          (Leaf 9))))
<        (Fork (Fork (Fork (Leaf 10)
<                          (Leaf 11))
<                    (Fork (Leaf 12)
<                          (Leaf 13)))
<              (Fork (Fork (Leaf 14)
<                          (Leaf 15))
<                    (Fork (Leaf 16)
<                          (Leaf 17))))

So graceful, blowing gently in the breeze.

So, if `>>=` replaces the leaves of a tree with
new trees, and `fmap` replaces all the values, we might
be able to imagine what `liftA2` does to combine two
trees. First `>>=` will take all the leaves of the left
tree, and with their values will map a function over the
right tree -- this is then grafted into the hole left behind.
For instance:

< liftA2 (,) (Fork (Leaf True) (Leaf False))
<            (Fork (Leaf 'a') (Leaf 'b'))
< =
<   Fork (Fork (Leaf (True,'a'))
<              (Leaf (True,'b')))
<        (Fork (Leaf (False,'a'))
<              (Leaf (False,'b')))

I encourage you to stare at this a while until you see how
the result would have come about. Take the definition of
liftM2 above and evaluate by hand if you need. As there
were two leaves in each tree, the applicative will multiply
them to produce a tree with `2*2=4` leaves.

Right, enough abstract monads. Let's print "hello world".

IO: Interacting with the Outside World
--------------------------------------
In this weeks PPT you'll to get explore both the
Maybe monad and the IO monad. `IO a` is a type which
abstracts *programs* of type `a`, that interact with
the outside world (gross).

For the first time, we aren't going to see the definition
but we will explore the *operations*.

< putStrLn :: String -> IO ()
< print :: Show a => a -> IO ()
< writeFile :: FilePath -> String -> IO ()

All these functions take an argument, and return an `IO ()`.
That is, a "program" which returns no result. We also
once inside IO, cannot leave. The only way we can work with
`IO`, is by using its `Functor`/`Applicative`/`Monad` interface.

The top-level function for any program, is called `main`:

< main :: IO ()
< main = putStrLn "hello world"

FINALLY!

Let's get more interesting.

< main :: IO ()
< main = do
<   putStrLn "hello world"
<   print t

In practice, we can do more than just scream into the void.

< getChar :: IO Char
< getLine :: IO String
< readFile :: FilePath -> IO String

These are all `IO` things, but they don't return `()`,
they return *something* else.

> greet :: IO ()
> greet = do
>   putStrLn "Hello, what's your name?"
>   name <- getLine
>   putStrLn ("Hey " ++ name ++ ", nice to meet you!")

Let's build another small program, and call it a day.

> ageGroup :: Int -> IO ()
> ageGroup age
>   | age < 11 = putStrLn "You are a child"
>   | age < 18 = putStrLn "You are a teenager"
>   | age < 25 = putStrLn "You are a young adult"
>   | age < 65 = putStrLn "You are a adult"
>   | otherwise = putStrLn "You've been around for a while"

> converse :: IO ()
> converse = do
>   greet
>   putStrLn "How old are you?"
>   age <- read <$> getLine
>   ageGroup age

Question Time
-------------
In the next lecture, I got some great questions. I'll summarise
the discussion here:

One asked how we can *conditionally* do something forever. We
can do this by using our trusty `if-then-else` construct:

> main :: IO ()
> main = wistfulness

> wistfulness :: IO ()
> wistfulness = do
>   putStrLn "we will miss you"
>   putStrLn "will you miss us?"
>   ans <- getLine
>   if ans == "yes" then pure ()
>   else wistfulness

by referring back to ourselves in the `else` case,
we ensure this function will run until the
`ans` is `"yes"`.

Another question asked if the shape of the
various operators in Monad and Applicative
have some intuition behind their shapes:
this is a *fantastic* question, and it turns
out they absolutely do! Let's see them *all*
together:

< (<$>) :: Functor t => (a -> b) -> t a -> t b
< (<$) :: Functor t => a -> t b -> t a
< (<*>) :: Applicative t => t (a -> b) -> t a -> t b
< (<*) :: Applicative t => t a -> t b -> t a
< (*>) :: Applicative t => t a -> t b -> t b
< (>>=) :: Monad m => m a -> (a -> m b) -> m b
< (>>) :: Monad m => m a -> m b -> m b

Note that `>>` and `*>` are the same thing.
So, when one of these operators has `<` and `>`,
it is saying it cares about both sides equally.
Notice that when they are imbalanced, one of the
results of the `t`/`m`s are just ignored. The
operators still *combine* the two structures, but
they just take the results from one of them.
This pretty much gives us `const` and `flip const`
for functor/applicative/monad things.

< [0, 1, 2, 3] *> [4, 5] = [4, 5, 4, 5, 4, 5, 4 5]

Bit weird when we work on lists (our list has 4 * 2 results, but
[4, 5] is just repeated), but it makes sense
for IO. For instance, when we compose two print
statements, we don't actually care about their
results, but we DO care about printing both things:
`print 1 *> print 2`.

For the intuition behind `>>=`'s shape, it's using the
arrow to point at "what comes next", but also the `=`
suggests that it will bind the result of the first
thing for us to use to determine the next thing (that's
where the name comes from, incidentally).