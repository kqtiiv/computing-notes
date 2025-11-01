This lecture we continue climbing up the tower of abstraction
from Functors.

> module Applicatives where

> import Control.Applicative

I want to start by just solidifying our understanding of
what it means to be a Functor. We met a few yesterday.

Functor Maybe, Functor (x ->), Functor [], Functor Tree...

All of these types support a mapping function, called `fmap`,
subject to the following "laws":

< fmap id = id
< fmap f . fmap g = fmap (f . g)

`fmap` promises to preserve the shape of the data it operates on,
only changing the values of type `a` hidden somewhere inside.
At the end of the last lecture, we met these two functions:

> addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
> addMaybe Nothing _ = Nothing
> addMaybe (Just x) my = fmap (x +) my

> addList :: [Int] -> [Int] -> [Int] -- cartesian-style
> addList [] _ = []
> addList (x:xs) ys = fmap (x +) ys ++ addList xs ys

These are *combining* two structures to make a new one:
our intuition is that they produce a *bigger* structure:

< addList [1, 2, 3] [100, 200, 300] = [101, 102, 103, 201, 202, 203, 301, 302, 303]

For two lists of size m and n, `addList` will produce a list of size `m * n`.
Indeed `addMaybe` does similar, except `Maybe` has either 0 or 1 `a`s inside.
We can either get 0 * 1, 1 * 0, 0 * 0 or 1 * 1 element Maybes out: 0 or 1.

Suppose we didn't want to "add" lists and maybes, what if we also wanted
to multiply them?

> mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
> mulMaybe Nothing _ = Nothing
> mulMaybe (Just x) my = fmap (x *) my

> mulList :: [Int] -> [Int] -> [Int] -- cartesian-style
> mulList [] _ = []
> mulList (x:xs) ys = fmap (x *) ys ++ mulList xs ys

Well, these are basically the same functions as before, but with `(*)` instead.
We know how to deal with this though, right, we create a *higher-order function*:

> productWithMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
> productWithMaybe f Nothing _ = Nothing
> productWithMaybe f (Just x) my = fmap (f x) my

> productWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
> productWithList f xs ys = [f x y | x <- xs, y <- ys]

Now, we can simplify our definitions:

< addList = productWithList (+)
< mulList = productWithList (*)
< addMaybe = productWithMaybe (+)
< mulMaybe = productWithMaybe (*)

This is still a *bit* of a shame though right, we've written two
versions of the add function, two versions of the mul function too.
If we could generalise `productWith`, we could do better!

We want to abstract these into a stricter typeclass than Functors,
which can work with more than one thing, namely, two. There is a
constraint of these so-called *Applicatives* that they are all Functors.
As we'll see, there is a law that states applicatives can implement
functors. Now... let's take this slowly and piece by piece, before
we see the whole glory of `Applicative` out in front of us:

We are looking for a way of combining two structures together,
we called with `productWith`, in the "real world" we call this idea
*lifting*. Indeed:

< liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c

This is the generalisation of `productWithMaybe` and `productWithList`.
It is saying we we can take a function on `a`s `b`s and `c`s, and make it
work over `t a`s, `t b`s, and `t c`s instead. In a similar vein, we can
think of `fmap` as having another name:

< liftA :: Applicative t => (a -> b) -> t a -> t b

It makes functions that work on `a`s and `b`s work on `t a`s and `t b`s.
We'll see now how we can implement this "fmap" for all `Applicative`s.
We will also see how with just `liftA2` and `fmap` at our disposal, we
can generalise to work for *all* `liftAn` with n > 2.

Let's reason about under what cases `liftA` can be expressed as a `liftA2`...
We know that `liftA2` allows us to combine two structures, which may well
expand its size. However, we also know that `liftA` (or `fmap`) cannot change
the size of the structure. Let's take lists as an example. When we `liftA2 f xs ys`,
the result list is `length xs * length ys`. If we know that `liftA f xs` always results
in a list of size `length xs`. If we know that `liftA` is a special case of `liftA2`,
this gives us the following equation to solve:

< length xs * length ys = length xs

For this equation to work out, we require that one of the lists we work on, `ys`,
has length 1. So, at this point we can at least reason that, *if* `liftA` can be
implemented in terms of `liftA2`, the size of one of the structures must be `1`.
In fact, this gives us a second method required for Applicatives:

< pure :: Applicative t => a -> t a

`pure x` is the "minimal" structure containing `x` for some applicative structure
`t`. In terms of the above discussion, `pure x` is guaranteed to be "size 1". That
helps us constrain the valid implementations, *and* gives us a generic way to refer
to structures of size 1. Now we can implement `liftA` in terms of `liftA2` and `pure`:

< liftA f mx = liftA2 (\f x -> f x) (pure f) mx

This definition is correct, but very clumsy looking! What is it saying?
To `fmap f` over an `mx`, we first wrap up `f` into the minimal structure
using `pure`. Then we use `liftA2` to combine the function in `pure f`
with the values in `mx`. The function we used for this was `\f x -> f x`,
which looks a bit weird. It has a name:

< ($) :: (a -> b) -> a -> b
< f $ x = f x

The eagle-eyed reader will note this is `($) = id`. It's weird, but sometimes
its useful. So, in other words:

< liftA f mx = liftA2 ($) (pure f) mx

The functions `liftA2` and `pure` together form `Applicative`, but there
is an alternative formulation with equal power. More classically, you might
see a shortening:

< (<*>) :: Applicative t => t (a -> b) -> t a -> t b -- pronounced "app"
< (<*>) = liftA2 ($)

Now, with this, we can *once again* rewrite liftA:

< liftA f mx = pure f <*> mx

This is one of the laws for Applicative, that `fmap` is the same as
"apping" a `pure f` to a structure. For this to work out, `pure`
must always return a structure that when used with `<*>` does not change
the "shape" of `mx` when it is mapped.

Ok, we are nearly at the point where we can look at the whole of
the `Applicative` class. However, it is worth introducing one more
alias for `fmap` (sorry, it's just how it is, `fmap` has three common
names it goes by), this one is symbolic, which allows it to be used
"nicely" alongside `<*>`:

< (<$>) :: Functor f => (a -> b) -> f a -> f b
< (<$>) = fmap

Just to reiterate: `fmap`, `liftA` and `(<$>)` are ALL DIFFERENT NAMES FOR THE
SAME THING. However, while both `fmap` and `(<$>)` are part of Functor, the
`liftA` actually more accurately belongs to `Applicative`. It is often used
as the "default" implementation of `fmap` (we'll see what I mean by that later).

Ok, so, I said above that `liftA2` and `(<*>)` are equivalent in power. We saw
how `liftA2 ($)` is `(<*>)`, but now we need to see how `liftA2` can be implemented
in terms of `(<*>)` and `pure`:

< liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
< liftA2 f mx my = pure f <*> mx <*> my

This definition is relying on partial application of `f` by each `(<*>)`.
With two arguments, we need two `(<*>)` to fully resolve. Indeed, here
is another (non-core) function arising from Applicative:

< liftA3 :: Applicative t => (a -> b -> c -> d) -> t a -> t b -> t c -> t d
< liftA3 f mx my mz = pure f <*> mx <*> my <*> mz

Three arguments, three (<*>)s. No prizes for guessing the shape of `liftA4`:

< liftA4 :: Applicative t => (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
< liftA4 f mu mx my mz = pure f <*> mu <*> mx <*> my <*> mz

Cool, so by being able to combine *two* `t`s, we can actually combine any number
of them! Of course, if you're eagle eyed, you'll spot the `pure f <*> mx` bonus
law. Normally, we write these "chains" of applicatives with a leading `(<$>)` instead:

< liftA4 :: Applicative t => (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
< liftA4 f mu mx my mz = f <$> mu <*> mx <*> my <*> mz

This would be idiomatic. Right, we've bootstrapped enough, we can *now* see
Applicative in its full glory:

< class Functor t => Applicative t where
<   pure :: a -> t a
<
<   (<*>) :: t (a -> b) -> t a -> t b -- pronounced "app"
<   (<*>) = liftA2 ($)
<
<   liftA2 :: (a -> (b -> c)) -> t a -> t b -> t c
<   liftA2 f mx my = f <$> mx <*> my -- we can only use `fmap` here because all applicatives must be functors.

This class looks a bit weird, right, because we have *both* `(<*>)` and
`liftA2` with definitions. Essentially, when we give the `Applicative`
instance we *MUST* provide one of the two of them. Which ever we provide,
we'll get the *other* for free.

While it is still true we can derive `Functor`, what you might see in the "real world",
is that people will write instances for `Functor` where `fmap = liftA`. Now that we
can derive Functor, this is less likely (unless the derivation doesn't work, it can't
define *every* functor instance for us!), but this "pattern" will appear again next week.
It looks like this:

< instance Functor Foo where
<  fmap = liftA
< instance Functor Bar where
<  fmap = liftA
< ...

We might assume that we could write something like this?

< instance Applicative f => Functor f where
<   fmap = liftA

This doesn't work, and it will make the typechecker fall over and die.
Other languages solve this problem of "free functor from applicative"
in better ways than Haskell does, however, this comes from trade-offs
in language design. Oh well.

We've been thinking very abstractly for a while now, so let's dip
back into the concrete and see some example instances (we already
have our functor instances, so we are ready to go!):

< instance Applicative Maybe where
<   pure :: a -> Maybe a
<   pure x = Just x
<
<   liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
<   liftA2 f Nothing _ = Nothing
<   liftA2 f _ Nothing = Nothing
<   liftA2 f (Just x) (Just y) = Just (f x y)

Let's see another example:

< instance Applicative [] where
<   pure :: a -> [a]
<   pure x = [x]
<
<   (<*>) :: [a -> b] -> [a] -> [b]
<   fs <*> xs = [f x | f <- fs, x <- xs]

Let's skip Tree, even though there is an `Applicative Tree` and `Applicative Bush`.
We can see the `pure`s as the idea of "minimal structure", but the `liftA2`/`(<*>)`
is *horrible* and not at all intuitive. We'll define them on Monday, when we have
the next level of abstraction down...

< data Tree a = Tip | Node (Tree a) a (Tree a)
< data Bush a = Leaf a | Fork (Bush a) (Bush a)

< pureTree :: a -> Tree a
< pureTree x = Node Tip x Tip

< pureBush :: a -> Bush a
< pureBush x = Leaf x

We will now suspend our discussion of trees.

Back to `Either`!

< data Either a b = Left a | Right b

< instance Functor (Either x) where
<   fmap :: (a -> b) -> Either x a -> Either x b
<   fmap f (Left x) = Left x
<   fmap f (Right y) = Right (f y)

< instance Applicative (Either e) where
<   pure :: a -> Either e a
<   pure y = Right y
<
<   liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
<   liftA2 f (Left x) _ = Left x
<   liftA2 f (Right x) (Left y) = Left y
<   liftA2 f (Right x) (Right y) = Right (f x y)

This one is interesting, because the `liftA2 f (Left x) _` made a *choice*.
We decided to favour the first `Left` and always ignore the second argument,
even if it were a `Left`. There are other applicative types that can combine
them, however (I've put that at the end, to help reinforce your understanding).
Effectively, the idea here is we can only combine two `Either`s when they are
both `Right`s. This again corresponds to an error condition.

Ok, we've got some instances, we've thoroughly explored the class and its
methods. Let's write a function using this! In this week's PPT you've been
implementing an evaluator. I'll take a subset of this and implement it,
but we'll change one of the core assumptions of the PPT.

> data Expr = Add Expr Expr | Var String

This is a small version of your `Expr` in the PPT. Your first task
was to write `eval`. However, in the PPT, it is assumed that the
variables you need to look up in the `[(String, Int)]` context was
always there. Instead, let's allow them to be missing! It is now
possible for looking up a variable to fail, so we will return `Maybe Int`
instead:

> evalMaybe :: Expr -> [(String, Int)] -> Maybe Int
> evalMaybe (Var v) ctx = lookup v ctx

Happily, it is actually easier now to write the `Var` case, because
`lookup` already returns a `Maybe`: it accounts for failure!
However, now that evaluating variables may fail, we need to write
our evaluation of `Add` in such a way as we account for the failures
in either sub-expression. Remember, what the inituition for Applicative
on Maybe is doing is allowing us to think about success and "ignore"
failure. Let's make the leap of faith:

> evalMaybe (Add e1 e2) ctx = (+) <$> evalMaybe e1 ctx <*> evalMaybe e2 ctx

Here we are saying: "evaluate both of e1 and e2 with the context. Then combine
these structures, if they are both `Just` (and therefore successful), combine
them with `(+)`, otherwise, the whole expression evaluation fails". This is a
really elegant way of handling the failure. Without applicatives, we would
have to resort to a mess like this:

< evalMaybe (Add e1 e2) ctx = case evalMaybe e1 ctx of
<   Nothing -> Nothing
<   Just m -> case evalMaybe e2 ctx of
<     Nothing -> Nothing
<     Just n -> Just (m + n)

Which is a lot harder to read, once you understand what the applicatives
are trying to "say". Now, if we look at some examples, we'll spot that
`Maybe` isn't really helpful:

< evalMaybe (Add (Var "x") (Var "y")) [("x", 2)] = Nothing

This failed, but where and why? If we use `Either String`, instead,
we might get something better:

> evalEither :: Expr -> [(String, Int)] -> Either String Int
> evalEither (Var v) ctx = case lookup v ctx of
>   Nothing -> Left (v ++ " is out of scope")
>   Just n  -> Right n

This time, we can formulate a sensible error message for missing
variables. Very *very* happily, the rest of the function is *identical*
to `evalMaybe`:

> evalEither (Add e1 e2) ctx = (+) <$> evalEither e1 ctx <*> evalEither e2 ctx

Again, the applicative for `Either` allows us to "assume success" and ignore
failure. This time however, the example is more sensible:

< evalEither (Add (Var "x") (Var "y")) [("x", 2)] = Left "y is out of scope"

Note that because of the left-biasing of `liftA2` defined above, this will
only return the left-most error:

< evalEither (Add (Var "y") (Var "z")) [] = Left "y is out of scope"

Ok, so this is where the original lecture ended. As one final note, I
introduced a problem which applicatives cannot solve for us yet, which
takes us into next week. Currently, we know we can combine structures,
be it lists, maybes, eithers, with a function returning values.
But for maybes and eithers, specifically, what
if the function we want to combine them with *also* can introduce failure,
how can we elegantly handle that.

To make this concrete:

< addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
< addMaybe = liftA2 (+)

All is well, we can add two things assuming they both succeeded.
But last lecture we met safe div:

< safeDiv :: Int -> Int -> Maybe Int
< safeDiv _ 0 = Nothing
< safeDiv m n = Just (div m n)

What if we want to combine two possibly failing computations
by using safeDiv? Let's look at the type of `liftA2 safeDiv`:

< liftA2 safeDiv :: Maybe Int -> Maybe Int -> Maybe (Maybe Int)

Ah. Our result here isn't just a `Maybe Int`, as perhaps we want,
but a `Maybe (Maybe Int)`. We cannot, with applicatives, get a
way of collapsing this nested structure down. To make any more
progess, we need *monads*.

Extras
------
Ok, so everything above concludes the stuff I talked about in this
lecture. Here I will tie up some other loose ends and give some more
examples, as well as abstract our `evalMaybe` and `evalEither` functions
to make it more reuable (why did we write almost the same thing twice!)

Firstly, let's see an applicative which also allows for failure, but
*combines* the error messages generated:

> data Error e a = Error [e] | Success a deriving (Functor, Show)

Let's write an `Applicative` instance:

> instance Applicative (Error e) where
>   pure :: a -> Error e a
>   pure x = Success x
>
>   liftA2 :: (a -> b -> c) -> Error e a -> Error e b -> Error e c
>   liftA2 _ (Error errs) (Error errs') = Error (errs ++ errs')
>   liftA2 _ (Error errs) _             = Error errs
>   liftA2 _ _            (Error errs') = Error errs'
>   liftA2 f (Success x)  (Success y)   = Success (f x y)

This is neat, because now we can combine errors if they happen
on both sides! Let's take `eval` and redo it *again*:

> evalError :: Expr -> [(String, Int)] -> Error String Int
> evalError (Var v) ctx = case lookup v ctx of
>   Nothing -> Error [v ++ " is out of scope"]
>   Just n  -> Success n
> evalError (Add e1 e2) ctx = (+) <$> evalError e1 ctx <*> evalError e2 ctx

Again, the last line of the function is *identical* to the others. Let's see
what it does with the example with two errors:

< evalError (Add (Var "y") (Var "z")) [] = Error ["y is out of scope","z is out of scope"]

Fantastic!

Ok, but, as we have seen, we keep writing mostly the same function every time.
Let's abstract a bit. If we can factor out the "strategy" for looking up
in the context, we will end up with the same function every time:

> eval :: Applicative t => Expr -> (String -> t Int) -> t Int
> eval (Var v) ctx = ctx v
> eval (Add e1 e2) ctx = (+) <$> eval e1 ctx <*> eval e2 ctx

By saying that the context is no longer `[(String, Int)]` and
instead saying it's a function that returns a `t Int`, we can
now do this evaluation for a variety of different "semantics".
Indeed:

< evalMaybe e ctx = eval e search
<   where search v = lookup v ctx
<
< evalEither e ctx = eval e search
<   where search v = case lookup v ctx of
<           Nothing -> Left (v ++ " is not in scope")
<           Just n  -> Right n
<
< evalError e ctx = eval e search
<   where search v = case lookup v ctx of
<           Nothing -> Error [v ++ " is not in scope"]
<           Just n  -> Success n

Fantastic!

Let's also take one final example to understand what the
role the applicative instance for lists plays. This time,
let's assume that our context returns a []: `String -> [Int]`.
We are basically saying that a variable could have no bindings,
but it could also have *more than one*. Let's implement the
eval function, then digest more what the consequences might be:

> evalList :: Expr -> [(String, Int)] -> [Int]
> evalList e ctx = eval e search
>   where search v = [n | (v', n) <- ctx, v == v']

This time, looking up in the context might return multiple
bindings. If it returns no binding for a variable, the result
is much like the `evalMaybe`:

< evalList (Add (Var "x") (Var "y")) [("x", 5)] = []

If every variable has a binding, we get:

< evalList (Add (Var "x") (Var "y")) [("x", 5), ("y", 3)] = [8]

But what if a variable has been given *two* or more assignments?

< evalList (Add (Var "x") (Var "y")) [("x", 5), ("y", 3), ("x", 10)] =
<   [8, 13]

Woah. With two valid definitions for `x`, our evaluator computed BOTH
5 + 3 *and* 10 + 3. Same evaluator code as for Maybe, Either String,
and Error String, but now we have multiple outcomes. Where Maybe, Either
and Error are all concerned with possibly failing computations, the
applicative for lists is concerned with *non-deterministic computations*.