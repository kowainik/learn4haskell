{- 👋 Welcome to Chapter Four / Four of our journey, Staunch Wanderer!

Our adventure together has almost come to an end. It has been a
magical era of mysteries and joy. But it is still no time to
relax. This is a critical chapter, where you finally will battle with
Monads! Mmm, exciting!

This Chapter requires the knowledge from the previous modules, so it
is highly encouraged to at least look through the material of those
chapters.

Let's also rewind how the training works.

== You are on __Chapter Four__.

In the final chapter, we are going to get acquainted with more
standard typeclasses that usually scare people off from Functional
Programming. Still, we hope that with all that we have been through,
these concepts won't frighten you anymore.

So, in this chapter, you are going to master:

 ✧ Kinds
 ✧ Functors
 ✧ Applicatives
 ✧ Monads

As usual, the explanations are in the Haskell comments of this
module. We are leaving a number of tasks on our path. Your goal is to
solve them all.

After finishing the PR, you can choose to summon us, @vrom911 and
@chshersh, to look at your solution in order to give some advice on
your code. This is optional; however, you can ask us for review only
if you want some feedback on your solutions.

Perfect. Let's crush this!
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs    #-}

module Chapter4 where

{- |
=🛡= Kinds

Before we attempt to defeat the mighty and scary Monad Dragon, we need
to talk about __kinds__. Understanding kinds is vital for the
wholesome perception of the Haskell type system.

All values in Haskell have types. But it turns out, types themselves
also have their own "types". Such "type of a type" is called __kind__.

Kinds describe the shape of a type. And kinds are much simpler than
types. Primitive types like 'Int' have kind * (star). You can check
information about kinds in GHCi using the ":k" command (short for ":kind"):

>>> :k Int
Int :: *

♫ NOTE: An alternative name for the star * kind is "Type". GHC is
  slowly replacing the naming of kind as * with "Type".

As data types have different types, in the same manner, there are more
than one kind of types. For example, data types like "Maybe" (that
have an additional type parameter) have a different kind. "Maybe" is
parameterised by a type variable. So, "Maybe" by itself doesn't have a
kind *. We call "Maybe" a __type constructor__. There are no values of
type "Maybe" without any parameters specified. As a consequence, you
can't write the following function:

@
foo :: Maybe -> Int
@

It is invalid, because "Maybe" needs some type parameters. In some
sense, you can look at "Maybe" as a function on type-level from types
to types: you give "Maybe" some type, and you get a concrete type in
the end. And that is the accurate description. Its kind is indeed a
function of two stars. You can check the kind of 'Maybe' in GHCi as
well:

>>> :k Maybe
Maybe :: * -> *
>>> :k Maybe Int
Maybe Int :: *

You can think of types with kind * as types in their complete final
form. Types of kind * have values. Types of any other kind don't have
values.

Haskell has one more standard kind — __Constraint__. This is the kind
for typeclasses. You can see ordinary types of kind * on both sides of
the function arrow (->) in type signatures. And you can see
constraints to the left side of the context arrow (=>).

You can check the kind of some standard typeclass like "Eq" to verify this:

>>> :k Eq
Eq :: * -> Constraint

We hope kinds will become your kind friends by the end of this chapter :)
-}

{- |
=⚔️= Task 1

Prepare for the first task! To complete the first challenge, you need
to explore kinds of different types in GHCi and insert the GHCi output
after each command.

As always, try to guess the output first! And don't forget to insert
the output in here:

>>> :k Char
Char :: *
>>> :k Bool
Bool :: *
>>> :k [Int]
[Int] :: *
>>> :k []
[] :: * -> *
>>> :k (->)
(->) :: * -> * -> *
>>> :k Either
Either :: * -> * -> *
>>> data Trinity a b c = MkTrinity a b c
>>> :k Trinity
Trinity :: * -> * -> * -> *
>>> data IntBox f = MkIntBox (f Int)
>>> :k IntBox
IntBox :: (* -> *) -> *

-}

{- |
=🛡= Functor

Let's continue our journey of exploring challenging concepts by
meeting 'Functor'. You may have heard of it and been told or imagined
that this is some Mathematical word, and there is nothing to do with
Functional Programming. But it is simpler.

'Functor' is just a typeclass. If we think about it from this
perspective, we would be able to objectively see what it could bring
to the FP world and us specifically, and stop labelling it Math
because of the name only.

And maybe we can start by looking at its definition?

@
class Functor f where
    fmap :: (a -> b) -> f a -> f b
@

As you can see, this is a class definition, but with a few more
challenging moments, we want to highlight in there.

First of all, you can see that 'Functor' has one method called 'fmap',
and it is a higher-order function — it takes a function from 'a' to
'b' as an argument, then something called 'f a', and returns 'f b'.

Before we dive into the details of 'Functor' implementation in
Haskell, let's try to see the meaning of this typeclass and its
method. And providing an analogy could help us with that.

'Functor' allows changing the value (and even its type) in some
context; we know this from the method's type signature. Let's say that
a chest is our context, and its content is the value inside that
context. You can replace the chest content with something else: you
can fill it with gold, you can take all the coins out the chest and
buy clothes using that money, and put back clothes in the chest
instead. Basically, you can replace the content of a chest with
anything else. This process of take-change-put is exactly what 'fmap'
describes!

That should make more sense now. But let's go back to Haskell. Functor
as a typeclass is defined for a type variable called 'f'. But from the
definition of 'fmap' you can notice that 'f' is not an ordinary
type. 'f' itself is parameterised by some type variable 'a' — 'f a'.

And in the light of the previous excursion into kinds, we can feel
that 'f' has a more complicated kind than 'Int' (* – star), for
example.

Indeed, you can check your suspicions by inspecting the kind of the
'Functor' typeclass in GHCi:

>>> :k Functor
Functor :: (* -> *) -> Constraint

Aha! So, we see that Functor works with types of the kind `* -> *`.
This already tells us a lot! For example, you can't implement
"instance Functor Int" (because Int has kind *), but you can implement
"instance Functor Maybe" (because Maybe has kind `* -> *`).

And, of course, the 'Functor' instance for 'Maybe' exists in the
standard Haskell library. Therefore, you can already use 'fmap' on any
Maybe value:

>>> fmap not (Just True)
Just False

Let's examine this one better. Maybe is parameterised by a single type
variable. This means that 'Maybe' can store "Int", "String", list, or
even other 'Maybe' of something inside. Basically, anything can be put
in there until it has the fittable kind.

In the example above, we use the 'not' function to 'fmap' our 'Maybe
Bool' value. But as 'not' doesn't change the type of Bool, then we get
'Maybe Bool' as the result of the 'fmap' in the example above.

That is a nice example, but not that interesting. Let's look at this one:

>>> fmap show (Just 42)
Just "42"
>>> fmap (replicate 3) (Just True)
Just [True,True,True]

Notice how the result type changes when we apply 'fmap' to one
argument at a time. Haskell has type inference, so it can infer proper
types and match them with actual types.

However, if we don't have a value inside, nothing changes. Or does it?

>>> fmap (replicate 3) Nothing
Nothing

Let's look at types closer:

>>> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
>>> :t fmap (replicate 3)
fmap (replicate 3) :: Functor f => f a -> f [a]
>>> :t Nothing
Nothing :: Maybe a
>>> :t fmap (replicate 3) Nothing
fmap (replicate 3) Nothing :: Maybe [a]


In GHCi we see 'Nothing', but it is not the same 'Nothing' that it's
been before. Its type has changed.

You can see how the 'Functor' instance for 'Maybe' is implemented. The
implementation uses good old pattern matching.

@
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
@

And as you see the underlying type of 'Maybe' changes over the 'fmap'.
That explains how that 'Nothing' changed its type in reality.

Now you see that Functors are not magical despite having a cryptic
name.

> QUESTION: Can you understand why the following implementation of the
  Functor instance for Maybe doesn't compile?

@
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f (Just a) = Just (f a)
    fmap _ x = x
@
-}

{- |
=⚔️= Task 2

Implement 'Functor' instance for the "Secret" data type defined
below. 'Secret' is either an unknown trap with something dangerous
inside or a reward with some treasure. You never know what's inside
until opened! But the 'Functor' instance allows changing the reward
inside, so it is quite handy.
-}
data Secret e a
    = Trap e
    | Reward a
    deriving (Show, Eq)


{- |
Functor works with types that have kind `* -> *` but our 'Secret' has
kind `* -> * -> *`. What should we do? Don't worry. We can partially
apply 'Secret' to some type variable that will be fixed inside each
method. Yes, similar to how we can partially apply functions. See, how
we can reuse already known concepts (e.g. partial application) from
values and apply them to the type level?
-}
instance Functor (Secret e) where
    fmap :: (a -> b) -> Secret e a -> Secret e b
    fmap _ (Trap e) = Trap e
    fmap f (Reward a) = Reward (f a)

{- |
=⚔️= Task 3

Implement Functor instance for the "List" type defined below. This
list type mimics the standard lists in Haskell. But for training
purposes, let's practise our skills on implementing standard
typeclasses for standard data types.
-}
data List a
    = Empty
    | Cons a (List a)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{- |
=🛡= Applicative

The 'Applicative' typeclass logically continues the 'Functor'
typeclass. Again, let's look at its definition first.

@
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
@

Wow, that's a lot going on again! Where did all these scary creatures
come? But if we look closer, it all looks a bit familiar already.

We now can spot straightaway, that similar to Functors, only types
with kind `* -> *` can have an Applicative instance (e.g. Maybe, and
not 'Int' or 'Char'), as we see the same 'f a' argument in the
methods' types.

But we also can notice the constraint in the class declaration. We
know that the constraint is the restriction on data types. And
actually, it works similarly in the typeclass declarations as
well. Putting this all together, it means that first, any data type
that wants to have an Applicative instance needs to implement the
'Functor' instance. This is what the part "Functor f => Applicative f"
means.

To be an Applicative, you first need to be "Functor". So, Functor is a
Level 1 creature, and Applicative is a Level 2 creature. Upgraded
Functor if you wish.

Unlike Functor, Applicative has two methods: 'pure' and operator (<*>)
called cyclops or starship or Sauron Eye. 'pure' puts a value into the
context, and (<*>) extracts function and value from the context,
applies the function to the argument, and puts the result back in the
context.

And continuing the chest 'Functor' analogy here, we can notice that a
chest is also an 'Applicative'! We can put anything we want inside our
chest, and this is what "pure" does. Or, let's say, we have one chest
with a key inside, and this key opens a very secret box inside another
chest. We can take both chests, extract their content, and apply one
to another (i.e. open the box with the key), extract the content of
the box and put it back to our chest. And this is the (<*>)
operator. So there are valid implementations of both methods of the
Applicative typeclass.

In pursuance of the above explanation, you probably can see now why it
is necessary to have 'Functor' first before becoming 'Applicative'. If
we can't replace the content of the chest with some other content, how
can we apply some actions to it and put something new inside?

Now, back to Haskell. The function 'pure' takes a value and puts in
the context 'f'. For 'Maybe' it looks like this:

>>> pure True :: Maybe Bool
Just True

The (<*>) operator is much juicier: it takes a function inside the
context, another value in the context, and returns a new value in the
same context. Apparently, the function somehow is extracted from the
context and applied to the extracted value.

To keep an eye (<*> hehe) on the next example, let's first check that
you still remember that in Haskell we have Higher-Order functions,
right? This means that functions can not only be passed as arguments
to data types but can also be stored inside other containers. Even
'Maybe' (e.g. "Maybe (Int -> Int)")!

>>> Just not <*> Just True
Just False
>>> Just (+ 4) <*> Just 7
Just 11
>>> Just (replicate 3) <*> Just 0
Just [0,0,0]

Can you guess what will happen if we try to apply (<*>) on 'Nothing'?
Exactly — 'Nothing'! Nothing will happen.

>>> Just not <*> Nothing
Nothing
>>> Nothing <*> Just True
Nothing

And, finally, we are ready to see how the 'Applicative' instance for
'Maybe' really looks like:

@
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Just (a -> b) -> Just a -> Just b
    Nothing <*> _ = Nothing
    Just f <*> x = fmap f x
@

So, even if the Applicative looks menacingly, there is Nothing scary
in it after all!

The next part might be a bit difficult to comprehend without having a
lot of practice with Applicatives, but it provides some deeper meaning
behind the concept.

If 'Functor' allows changing values inside the context, 'Applicative'
enables chaining operations. You may reasonably wonder how often we
have functions inside 'Maybe'. But remember, that in Haskell we have
partial application. This means that we can apply binary functions
partially to some values inside a context, and get a function inside
the context that requires only one argument.

Now, let's think, how can we implement a function that takes two
optional integers, and returns an optional integer with the sum of the
given integers inside? In other words, we want a function with the
following type signature:

@
addMaybes :: Maybe Integer -> Maybe Integer -> Maybe Integer
@

At this point you probably can implement it using pattern matching,
but can you do it using the 'Applicative' instance for 'Maybe'? It
turns out, this is possible!

@
addMaybes m1 m2 = fmap (+) m1 <*> m2
@

Let's disenchant this magic spell step-by-step.

ghci> :t +d (+)
(+) :: Integer -> Integer -> Integer
ghci> :t +d fmap (+)
fmap (+) :: Functor f => f Integer -> f (Integer -> Integer)
ghci> :t +d fmap (+) (Just 3)
fmap (+) (Just 3) :: Maybe (Integer -> Integer)

You see that by applying 'fmap' to (+) and some 'Maybe' we get a value
of type "Maybe (Integer -> Integer)". And this means that we can use
the '(<*>)' operator to combine it with another "Maybe Integer".

The beautiful thing about 'Applicative' is that scales over functions
of any number of arguments. The following code is valid, if the
function 'f' takes `x` arguments.

@
fmap f m1 <*> m2 <*> m3 <*> ... <*> m_x
@

Applicatives can be found in many applications:

 ✦ Chaining operations over optional values
 ✦ Parsers
 ✦ Input form validation
 ✦ Concurrent and parallel execution of tasks
-}

{- |
=⚔️= Task 4

Implement the Applicative instance for our 'Secret' data type from before.
-}
instance Applicative (Secret e) where
    pure :: a -> Secret e a
    pure = Reward

    (<*>) :: Secret e (a -> b) -> Secret e a -> Secret e b
    Trap e <*> _ = Trap e
    Reward f <*> x = fmap f x

{- |
=⚔️= Task 5

Implement the 'Applicative' instance for our 'List' type.

🕯 HINT: in the applicative instance for lists, you have a list of
  functions and a list of arguments for those functions. You need to
  apply each function to each argument and combine all the results. You
  may also need to implement a few useful helper functions for our List
  type.
-}

instance Applicative List where
    pure :: a -> List a
    pure x = Cons x Empty

    (<*>) :: List (a -> b) -> List a -> List b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Cons f fs <*> l = append (fmap f l) (fs <*> l)

append :: List a -> List a -> List a
append Empty ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

{- |
=🛡= Monad

Now, the Monad Dragon. We've come that far not to give up. If we
managed to fight 'Functor' and 'Applicative', then sure, we can beat
'Monad', right?💪

As usual, we need to know with whom we are dealing, so let's inspect
the 'Monad' typeclass definition:

@
class Applicative f => Monad f where
    (>>=) :: f a -> (a -> f b) -> f b
@

Look, it is just a simple typeclass! Also, it looks very familiar to
'Functor' and 'Applicative'. You can even start thinking that they are
all here just to confuse developers by moving arrows and 'f' letters
from one place to another.

First of all, to become a 'Monad', a Level 3 creature, a type must be
an 'Applicative' first. Only after that, it can even dare to implement
the (>>=) operator called __bind__.

*And this is again our reporter from the Chest Analogy show live.*
 Turns out, our chest is also a monad! If our chest contains some
 gold, we can take all our gold and buy a new chest using our
 gold. The remaining gold can be put to the new chest. The amount of
 money we have determines the quality of our new chest. And this is
 what the monad about — next context can depend on the value in the
 current context.

And to expand a bit more on why you need to have the 'Applicative'
instance before becoming a 'Monad': if you can't put values (stuff)
inside a context (chest) using "pure", then there is no sense in
buying a new chest at all.

So, our chest is 'Functor', 'Applicative' and 'Monad'. Pure gold example!

To describe the same in more technical words, The bind (>>=) operator
takes a value of the type 'f a' ('a' in the 'f' context), a function
from 'a' to 'f b' ('b' in the 'f' context), and returns a value of
type 'f b'. So, to understand what it means, let's get back to our
example with 'Maybe'. But first, we need to get somewhere a function
that we would be able to use for the second argument of (>>=) in our
examples.

Let's implement a "safe" 'half' function, that divides a number by 2,
but only if the number is even.

-}
half :: Int -> Maybe Int
half n
    | even n = Just (div n 2)
    | otherwise = Nothing

{- |

Now, we can experiment with this function and the 'Monad' instance of
'Maybe' in GHCi:

>>> Just 6 >>= half
Just 3
>>> Just 3 >>= half
Nothing
>>> Nothing >>= half
Nothing

That makes sense — the resulting context depends on the value in the
initial context.

Let's now see how it is implemented. The instance for 'Maybe' is
rather straightforward:

@
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f = f x
@

Look, it is even simpler than the 'Applicative' instance! I feel
deceived now. I heard legends about merciless Monads, scary stories
when I was a kid, but in reality, there is Nothing special in it!
Could I even name myself a Monad conqueror now? (Of course, you can,
but after you try to implement the instances in the exercises)

On the general note, you can notice some similarities between the main
methods of all three typeclasses:

@
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: (a -> f b) -> f a -> f b
@

♫ NOTE: The (=<<) operator is a version (>>=) with arguments
  flipped. It could be implemented via the regular bind operator.

All type signatures look similar, but they represent different
concepts in the end.
-}

{- |
=⚔️= Task 6

Implement the 'Monad' instance for our 'Secret' type.
-}
instance Monad (Secret e) where
    (>>=) :: Secret e a -> (a -> Secret e b) -> Secret e b
    Trap e >>= _ = Trap e
    Reward a >>= f = f a

{- |
=⚔️= Task 7

Implement the 'Monad' instance for our lists.

🕯 HINT: You probably will need to implement a helper function (or
  maybe a few) to flatten lists of lists to a single list.
-}

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    l >>= f = concatList (fmap f l)

concatList :: List (List a) -> List a
concatList Empty = Empty
concatList (Cons x xs) = append x (concatList xs)

{- |
=💣= Task 8*: Before the Final Boss

So far we've been talking only about instances and use cases of
different typeclasses. But one more cool thing we haven't discussed
yet is the ability to implement functions polymorphic over the
context. Let's say that you have two boolean values inside some
contexts, and you want to apply the logical AND operation on them. But
you don't know the context! It can be 'Maybe', or 'Either' or 'List',
or anything else! However, this is not a problem in Haskell. You still
can implement a function with the type signature described below.

Can you implement a monad version of AND, polymorphic over any monad?

🕯 HINT: Use "(>>=)", "pure" and anonymous function
-}
andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM ma mb = ma >>= \a -> if a then mb else pure False

{- |
=🐉= Task 9*: Final Dungeon Boss

You did it! You made it to the end of Haskell practice! It probably
was challenging, but you made it nevertheless! This makes us so happy
and proud of you!

Hope you enjoyed learning functional concepts and will continue it in
the future!

Let us know how did you like the journey and how we can make it even
more remarkable for you! This will take you a few minutes, but is
precious for us and future journey starters:

 * https://docs.google.com/forms/d/e/1FAIpQLScBVhLxq5CgGnAfIGUE-fCoOUqeGkDY2HXzbT7KV2jjLOsmjQ/viewform


Also, challenge your friends with this course and spread the word about us!
We are @kowainik on Twitter. You can also use #Learn4Haskell hashtag to share
your story.

We also have a Ko-fi page, if you want to buy us a coffee after a long road.
 ☕️ https://ko-fi.com/kowainik

You can also support creators of this course and your proud mentors on GitHub:
  ✧ https://github.com/sponsors/vrom911
  ✧ https://github.com/sponsors/chshersh

Now, for the desert, it is time to test your skills on the final boss!
Your task now will be to implement a Binary Tree data structure and
some functions on it.

Specifically,

 ❃ Implement the polymorphic binary tree type that can store any
   elements inside its nodes
 ❃ Implement the Functor instance for Tree
 ❃ Implement the reverseTree function that reverses the tree and each
   subtree of a tree
 ❃ Implement the function to convert Tree to list
-}


{-
You did it! Now it is time to the open pull request with your changes
and summon @vrom911 and @chshersh for the review!
-}

data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

reverseTree :: Tree a -> Tree a
reverseTree Leaf = Leaf
reverseTree (Node x l r) = Node x (reverseTree r) (reverseTree l)

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node x l r) = x : treeToList l ++ treeToList r
