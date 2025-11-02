This lecture is about datatypes and trees

> module Trees where

Let's start by reviewing `data`:

< data Nat where
<   Z :: Nat
<   S :: Nat -> Nat
<   deriving Show

Natural numbers have `Z`, which is a value, and `S`, which is a constructor:
it represents (+1). Technically, this syntax is the "new-style" for data,
the original is:

< data Nat = Z | S Nat

This is exactly the same. It is more concise, so let's take the training wheels
off and use this syntax exclusively. By the end of this lecture, we will hopefully
feel more comfortable with it. Let's remind ourselves about the anatomy of lists
(remember, this is "fake" syntax, as list is special and built-in):

< data [a] = [] | (a : [a])

We have an empty list and we think of `(:)` as adding something to the
"front" of an existing list. Thinking a bit more concretely, GHC
doesn't actually care what direction the list grows in: it is *us* that
decides we want to think of it being at the front. We feel this way
because of our natural intuition.

We are going to explore programmatic horticulture. Since we need
to make unique names for things, let's distinguish between two
types of "plants": Bushes and Trees. In the live lecture, I got myself
in a twist and mixed them up. To save myself from further confusion,
I will fix my mistake here:

* A Tree will have values in amongst the branches (like fruit)
* A Bush will have values at the ends of the tree (like berries)

I'm pretty sure this is not a *common* naming distinction, and in the
"real world", everyone will just say these are both just trees.
Let's start with bushes:

> data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving Show

Let's build some bushes:

> b1 :: Bush Int
> b1 = Fork (Leaf 5) (Leaf 6)

> b2 :: Bush Int
> b2 = Leaf 7

> b3 :: Bush Char
> b3 = Leaf 'a'

> b4 :: Bush Bool
> b4 =
>   Fork
>     (Fork (Leaf True)
>           (Leaf False))
>     (Leaf True)

This is not a bush, this is a mess:

< mess = (((Fork Fork) Leaf) True) Leaf False Leaf True

Someone asked if we could have forks with only one sub-tree in them.
To do this, we need more constructors, and new names. Coming up
with plant-related names is a fun exercise, so here
is a "shrub":

> data Shrub a = Twig a
>              | Branch (Shrub a)
>              | Bough (Shrub a) (Shrub a)

In practice, "shrubs" are just another kind of tree.
In fact, we can also think of lists as just being a tree
that only grows in one direction (to the right); perhaps
lists are growing towards the light? (see the attached PDF
with the diagrams)

Let's now take a look at `Tree`:

> data Tree a = Tip
>             | Node (Tree a) a (Tree a) -- a "node" is the forking point of many plants, it is also the best place to prune them...
>             deriving Show

> t1 :: Tree Int
> t1 = Node Tip 7 Tip

> t2 :: Tree a
> t2 = Tip

> t3 :: Tree Char
> t3 = Node (Node Tip 'b' Tip) 'a' Tip

Functions on Plants
-------------------
Now we have our different kinds of trees, we can now
start writing functions over them. The only tool
we have at our disposal for working with these is
going to be pattern matching.

< data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving Show

We'll focus on Trees primarily, but let's give Bush
a chance first:

Let's start by finding the number of elements in a bush, which
is the number of leaves. We do this recursively, with pattern
matching.

> sizeBush :: Bush a -> Int
> sizeBush (Leaf _) = 1                             -- every leaf only has a single value.
> sizeBush (Fork lt rt) = sizeBush lt + sizeBush rt -- forks do not contain values, but their subtrees will.

The depth of a bush/tree is the length of the longest back from the
root of the tree down to its terminal leaves. We will say a leaf has
depth 0, semi-arbitrarily. The depth of a fork is the greater of the
depths of the left and right bushes, with one added for this fork.

> depthBush :: Bush a -> Int
> depthBush (Leaf _) = 0
> depthBush (Fork lt rt) =
>   max (depthBush lt) (depthBush rt) + 1

A bush or a tree is perfect when every path from root to leaves is the
same depth.

> perfect :: Bush Int
> perfect = Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4))

When a bush or a tree is perfect, it will have depth `n`, and its
size will be `2^n`. This is because every level of the bush or tree
will double the number of sub-trees. This is a useful property, but
we won't use it in this course.

If we want, we can pluck off all the leaves of the bush and fill a
list with them all:

> valuesBush :: Bush a -> [a]
> valuesBush (Leaf x) = [x]                                 -- we have only one value here
> valuesBush (Fork lt rt) = valuesBush lt ++ valuesBush rt  -- we have no values in forks, but the subtrees will.

That's enough bushes. Let's spend some more time looking at trees.

< data Tree a = Tip
<             | Branch (Tree a) a (Tree a)

Like with bushes, we can harvest the fruit in the tree into a list:

> valuesTree :: Tree a -> [a]
> valuesTree Tip = []
> valuesTree (Node lt x rt) =
>   valuesTree lt ++ [x] ++ valuesTree rt

This time, nothing is found at the tips of the tree's branches, and
the nodes will put their value between the values from the left and
right. Obviously, we could have included `x` in any order here, but
chosing this way will be useful...

Let's establish a convention. For a Tree, we will expect to find
smaller things on the left of the tree and larger things on the
right. For any node `Node lt x rt`, we expect everything in `lt`
to be less-than-or-equal to `x`, and everything in `rt` to be
greater than `x`. With this convention, we can note that `valuesTree`
will always return a sorted list.

Last lecture, we saw the `elem` function, which asks if an element
can be found in a list. Because we are assuming that our trees
will have a nice consistent ordering, we can also query them
to find whether something is inside or not. We'll call this
function `member`:

> -- pre: nodes always have smaller values to the left and larger to the right
> member :: Ord a => a -> Tree a -> Bool
> -- trivially, `x` is not found in a `Tip`.
> member x Tip = False
> -- otherwise, let's use our compare function from yesterday. We consider 3 cases:
> member x (Node lt y rt) = case compare x y of
>   LT -> member x lt  -- if x < y, then given our pre-condition `x` could be found somewhere on the LEFT
>   EQ -> True         -- if x == y, then x is clearly found in the tree
>   GT -> member x rt  -- if x > y, then our pre-condition suggests `x` could be found on the RIGHT

The pre-condition of orderedness was useful here. By assuming
the property, our algorithm resembles a binary-search. As we learnt
earlier, a perfect tree with `2^n` elements will have depth `n`.
In other words a perfect tree with `n` elements will have depth `log n`.
This means we need to walk down at most `log n` nodes of a perfect tree
before we find our answer. HOWEVER, this is assuming the trees are
always perfect, which they are not necessarily. Constructing trees
that are guaranteed to be nicely balanced is left for next year.

Moving on, it would be nice to have a function to insert a value into
a tree whilst preserving the orderedness property we used to define
a nice `member` function. Let's call that `insert`:

> -- pre: the tree is nicely ordered, as before
> insert :: Ord a => a -> Tree a -> Tree a
> -- Tips can't accommodate values; we'll need to build a Node with two Tips instead.
> insert x Tip = Node Tip x Tip
> -- inserting in our Node, we must be careful to preserve orderedness.
> insert x (Node lt y rt)
>   -- if x <= y, it belongs on the left (we allow duplicates). Push that down to the nearest Tip
>   -- and rebuild the rest of the tree around it
>   | x <= y = Node (insert x lt) y rt
>   -- otherwise, same thing, but push it down to the Tips on the right!
>   | otherwise = Node lt y (insert x rt)

Again, while this preserves the orderedness of the trees, it does not
guarantee the tree will be balanced. That is left for next year.
Let us continue our gardening; can we grow a tree now?
The function `grow` will take a list of values and build a
`Tree` with those values in them. This resulting tree will have
the orderedness property, thanks to `insert`.

> grow :: Ord a => [a] -> Tree a
> --grow [] = Tip
> --grow (x:xs) = insert x (grow xs)
> grow = foldr insert Tip

Squint and we will spot that a `foldr` has appeared!
As the tree is ordered, this `grow` function has naturally
sorted the list; isn't nature wonderful. We can now define
a full sorting algorithm using this:

> tsort :: Ord a => [a] -> [a]
> tsort = valuesTree . grow

This algorithm is no better than insertion sort.
Why? These trees can be biased to the left or
right, and that makes one or both of `grow` and
`valuesBush` linear. If the trees were guaranteed
to be balanced, this would be a great sorting algorithm.

Rose bushes
-----------
We would like some more flexibility in our bushes.
What if they can branch arbitrarily. This is
classically called a "rose-bush".

> data Rose a = Flower a
>             | Vine [Rose a]

A rose-bush has as many splits as we want at each level:

> r1 :: Rose Int
> r1 = Vine [ Flower 4
>           , Vine [ Flower 5
>                  , Flower 3
>                  ]
>           , Vine []
>           ]

You might recognise this kind of structure from your PPTs
this week: the `Command` type is a rose-bush, where the
branches are `B`s and there are flowers shaped like `F`, `L`, `R`.
Many strange plants appear in the wild...

Plucking the roses from the bush requires a bit more care this time
(and not just because of the thorns!). This time we need to collect
the values from each rosebush within a `Vine`, and then flatten these
lists down. Thankfully, we have a recipe for that from last week:

> valuesRose :: Rose a -> [a]
> valuesRose (Flower x) = [x]
> valuesRose (Vine ts) = concatMap valuesRose ts

Let's see how this operates on `r1` above:

< valuesRose r1 = valuesRose (Vine [Flower 4, Vine [Flower 5, Flower 3], Vine []])
<               = concatMap valuesRose [Flower 4, Vine [Flower 5, Flower 3], Vine []]
<               = concat [valuesRose (Flower 4), valuesRose (Vine [Flower 5, Flower 3]), valuesRose (Vine [])]
<               = valuesRose (Flower 4) ++ concat [valuesRose (Vine [Flower 5, Flower 3]), valuesRose (Vine [])]
<               = [4] ++ concat [valuesRose (Vine [Flower 5, Flower 3]), valuesRose (Vine [])]
<               = 4 : concat [valuesRose (Vine [Flower 5, Flower 3]), valuesRose (Vine [])]
<               = 4 : valuesRose (Vine [Flower 5, Flower 3]) ++ concat [valuesRose (Vine [])]
<               = 4 : concatMap valuesRose [Flower 5, Flower 3] ++ concat [valuesRose (Vine [])]
<               = 4 : concat [valuesRose (Flower 5), valuesRose (Flower 3)] ++ concat [valuesRose (Vine [])]
<               = 4 : valuesRose (Flower 5) ++ concat [valuesRose (Flower 3)] ++ concat [valuesRose (Vine [])]
<               = 4 : [5] ++ concat [valuesRose (Flower 3)] ++ concat [valuesRose (Vine [])]
<               = 4 : 5 : concat [valuesRose (Flower 3)] ++ concat [valuesRose (Vine [])]
<               = 4 : 5 : valuesRose (Flower 3) ++ concat [] ++ concat [valuesRose (Vine [])]
<               = 4 : 5 : [3] ++ concat [] ++ concat [valuesRose (Vine [])]
<               = 4 : 5 : 3 : concat [] ++ concat [valuesRose (Vine [])]
<               = (4 : 5 : 3 : []) ++ concat [valuesRose (Vine [])]
<               = 4 : 5 : 3 : concat [valuesRose (Vine [])]
<               = 4 : 5 : 3 : (valuesRose (Vine []) ++ concat [])
<               = 4 : 5 : 3 : (concatMap valuesRose [] ++ concat [])
<               = 4 : 5 : 3 : (concat [] ++ concat [])
<               = 4 : 5 : 3 : ([] ++ concat [])
<               = 4 : 5 : 3 : concat []
<               = 4 : 5 : 3 : []

Phew.

Trees are not just used for ~christmas~ values, they can encode actual
things in their shapes. The PPT Command tree is an example, where we
don't really have nodes, more that the tips have varying forms that
each mean a different thing. Here is another example, from next week:

> data Expr = Val Int
>           | Var String
>           | Add Expr Expr
>           | Mul Expr Expr
>           | Neg Expr

You will write this next week: >:)

< eval :: Expr -> [(String, Int)] -> Int
< eval = ...

Here are some expressions

> e1 = Val 5      -- represents 5
> e2 = Var "x"    -- represents x
> e3 = Add e1 e2  -- represents 5 + x
> e4 = Mul e1 e3  -- represents 5 * (5 + x)

We have no representation of brackets in this tree,
the bracketing comes from which nodes are closer to the root.

<    = Mul (Val 5) (Add (Val 5) (Var "x"))

For contents sake, let's make something else. This is a tree
that represents boolean predicates:

> data Pred = T | F          -- true and false
>           | And Pred Pred
>           | Or Pred Pred
>           | Not Pred
>           | Less Expr Expr

This tree has a fork that goes off to `Expr`! In fact, we could
add an `If Pred Expr Expr` constructor to `Expr` if we wanted,
and we'd have a *mutally-recursive* datastructure. We would
be processing that with a mutually recursive pair of functions.
Let's see an example predicate:

> p1 = Or (Less (Val 6) (Var "x")) (Less (Var "x") (Val 10))
>    --  (6 < x) || (x < 10)

We can write a recursive function to evaluate these predicates.
For consistency with `eval`, which takes a mapping of variables
to their values, we'll need to thread a similar thing through,
though we don't use it ourselves. This is called an "environment".
The function is recursive:

< evalPred :: Pred -> [(String, Int)] -> Bool
< evalPred T env = True
< evalPred F env = False
< evalPred (And p q) env = evalPred p env && evalPred q env
< evalPred (Or p q) env = evalPred p env || evalPred q env
< evalPred (Not p) env = not (evalPred p env)

In the last case, this is where we would call the `eval` function
to process an `Expr` into an `Int`. Now we can use `<` on those
results to get our `Bool` back. If `Expr` had the `If`, we would
then have `eval` calling `evalPred`, for the mutual recursion.

< evalPred (Less x y) env = eval x env < eval y env

Let's ~leaf~ leave that there for now.

At some point during the lecture, someone asked if we can map
a function over a tree. This is a great question! Trees, Bushes,
etc are not built into Haskell, so we would need to define our
own version:

< data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show

The promise that `map` made on lists was that it would neither
increase or decrease the size of the list, and just change the
values. When thinking about mapping on trees, we have a similar
assertion; this time though, we promise to not reorder the
branches either. Values are only found in the `Node`s, so nothing
to do for the `Tip`s; in the `Node`s however, we need to apply
a function to the middle, and map fully the left- and right-hand
sides:

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f Tip = Tip
> treeMap f (Node lt x rt) =
>   Node (treeMap f lt) (f x) (treeMap f rt)

We now have a way to map trees:

<             t3 = Node (Node Tip 'b' Tip) 'a' Tip
< treeMap ord t3 = Node (Node Tip 98  Tip) 97  Tip

Cool! As we'll see next week, there is a generalisation
for `map`-like functions. It is a typeclass, and it
is also derivable :)