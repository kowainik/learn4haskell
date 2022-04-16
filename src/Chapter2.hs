{- 👋 Welcome to Chapter Two, Traveller!

If you haven't finished Chapter One yet, we encourage you to check it
out, as the knowledge from the previous section is required in order
to pass this training.

If you already finished Chapter One — congratulations and huge
respect! We are happy to see you with us at the next step.

A casual reminder of how it works.

== You are on __Chapter Two__.

Here we will explore more interesting functional concepts. We are
going to practice our skills on the list type in Haskell.

In this chapter, you are going to master:

 ✧ The list data type
 ✧ Immutability
 ✧ Pattern-matching
 ✧ Recursion
 ✧ Parametric polymorphism
 ✧ Lazy evaluation
 ✧ Higher-order functions
 ✧ Partial function application
 ✧ Eta-reduction

As usual, the explanations are in the Haskell comments of this
module. We are leaving a number of tasks on our path. Your goal is to
solve them all and make the tests for Chapter Two green.

After finishing the PR, you can choose to summon us, @vrom911 and @chshersh,
to look at your solution in order to give some advice on your
code. This is optional; however, you can ask us for review only if you
want some feedback on your solutions.

Now, if you are ready, bring it on!
-}

module Chapter2 where

{-
=🛡= Imports

Before we start diving into the FP concepts, let's cover an important
process-development detail.

Without any additional effort, in Haskell you have access to all
functions defined in the special module called "Prelude". This module
is always imported by default and contains all primitive data types
and useful functions to work with them. However, sometimes you need to
import other modules even from other libraries.

The Haskell Standard library is called "base". It provides modules to
work with different data types and values. If you want to bring some
additional types and functions in scope, you need to import them
manually.

For example, to import the list sorting function, you need to write:

@
import Data.List (sort)
@

All imports should go at the beginning of the module: after the
"module MODULE_NAME where" line and before the first function (or
type) definition.

♫ NOTE: you can use Hoogle to explore modules and functions from other
  places as well.

When working with lists, the most practical module will be "Data.List":

   * https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
-}


{- |
=🛡= Lists

__List__ is a crucial data type in Haskell and functional programming
in general. It represents a collection of elements of the _same type_.
The list type is written as the type of list element in square
brackets. For example, a list of integers will have type '[Int]' and a
list of booleans — '[Bool]'.

[Interesting fact]: String in Haskell is a list of characters ('Char'
data type in Haskell) and is written as '[Char]'. But Haskell also
provides the "String" alias to '[Char]'. So, in some places, you will
see 'String', and in others, you will see '[Char]', but they mean the
same thing. We will explain better how it works in Chapter Three. For
now, you only need to know that you can use 'String' and '[Char]'
interchangeably.

To create a list, you need to put elements in square brackets (the
same as used for the list type) and separate them by commas. Such
expressions are called __list literals__. For example, the expression
"[3, 5, 1]" creates a list of three numbers, where the first list
element is number 3, and the last element is number 1. Similarly, you
can create a list of two booleans: [False, True]. A list without
elements is just [].

You probably noticed that lists could be of any type of
elements. Often you want to write a function that works with lists of
any type (but consistent inside one list). This feature is called
__parametric polymorphism__. It will be explained in more detail
later, but when working with lists, you often will see type signatures
like:

@
foo :: [a] -> [b] -> [a]
@

The above type signature means that this function takes two lists: one
with elements of some type "a" and another with elements of type "b"
(the function doesn't care about the specific types) and returns the
list with elements of the same type as the first list. Such words "a"
and "b" are called __type variables__.

For comparison, specific types in Haskell start with an uppercase
letter (Int, Bool, Char, etc.), where type variables begin with a
lowercase letter (a, b, el, etc.). This is the way to distinguish
between these types.

The Haskell standard library already provides a lot of functions to
work with lists. And you will need to operate a lot with standard
functions in the upcoming exercises. Remember, Hoogle is your friend!
-}

{- |
=⚔️= Task 1

Explore lists by checking types of various list expressions and
functions in GHCi and insert the corresponding resulting output below:

List of booleans:
>>> :t [True, False]
[True, False] :: [Bool]

String is a list of characters:
>>> :t "some string"
"some string" :: [Char]

Empty list:
>>> :t []
[] :: [a]

Append two lists:
>>> :t (++)
(++) :: [a] -> [a] -> [a]

Prepend an element at the beginning of a list:
>>> :t (:)
(:) :: a -> [a] -> [a]

Reverse a list:
>>> :t reverse
reverse :: [a] -> [a]

Take first N elements of a list:
>>> :t take
take :: Int -> [a] -> [a]

Create a list from N same elements:
>>> :t replicate
replicate :: Int -> a -> [a]

Split a string by line breaks:
>>> :t lines
lines :: String -> [String]

Join a list of strings with line breaks:
>>> :t unlines
unlines :: [String] -> String

-}

{- |
=⚔️= Task 2

To understand the list type better, it is also beneficial to play with
list expressions in REPL.

Evaluate the following expressions in GHCi and insert the answers. Try
to guess first, what you will see.

>>> [10, 2] ++ [3, 1, 5]
[10,2,3,1,5]
>>> [] ++ [1, 4]  -- [] is an empty list
[1,4]
>>> 3 : [1, 2]
[3,1,2]
>>> 4 : 2 : [5, 10]  -- prepend multiple elements
[4,2,5,10]
>>> [1 .. 10]  -- list ranges
[1,2,3,4,5,6,7,8,9,10]
>>> [10 .. 1]
[]
>>> [10, 9 .. 1]  -- backwards list with explicit step
[10,9,8,7,6,5,4,3,2,1]
>>> length [4, 10, 5]  -- list length
3
>>> replicate 5 True
[True,True,True,True,True]
>>> take 5 "Hello, World!"
"Hello"
>>> drop 5 "Hello, World!"
", World!"
>>> zip "abc" [1, 2, 3]  -- convert two lists to a single list of pairs
[('a',1),('b',2),('c',3)]
>>> words "Hello   Haskell     World!"  -- split the string into the list of words
["Hello","Haskell","World!"]


👩‍🔬 Haskell has a lot of syntax sugar. In the case with lists, any
  list literal like "[3, 1, 2]" is syntax sugar for prepending elements
  at the empty list: "3 : 1 : 2 : []".

Don't forget that lists are the containers of the same-type
elements. Meaning, you can't combine lists of different types in any
situation. Let's try appending a list of booleans and a string (list
of characters) to see the error message:

ghci> [True, False] ++ "string"
<interactive>:4:18: error:
    • Couldn't match type ‘Char’ with ‘Bool’
      Expected type: [Bool]
        Actual type: [Char]
    • In the second argument of ‘(++)’, namely ‘"string"’
      In the expression: [True, False] ++ "string"
      In an equation for ‘it’: it = [True, False] ++ "string"

-}

{- |
=🛡= Immutability

At this point in our training, you need to learn that all values in
Haskell are immutable! Woohoo! But what does it mean for us?

It means that when you apply a function to some variable, the value is
not changed. Instead, you create a new value each time.

>>> import Data.List (sort)  -- sort is not in Prelude
>>> x = [3, 1, 2]  -- you can assign values to variables in GHCi
>>> sort x
[1,2,3]
>>> x
[3,1,2]

The 'sort' function returns a new sorted list. It doesn't change the
original list, so you don't need to worry about accidentally spoiling
values of variables you defined before.
-}

{- |
=🛡= List implementation

Let's talk a bit about list implementation details. Lists in Haskell
are implemented as __linked lists__ (or cons-lists). And because
everything in Haskell is immutable, adding elements at the beginning
of the lists is cheap. Haskell doesn't need to allocate new memory and
copy the whole list there; it can just create a new list from a new
element and a pointer to an already existing list. In other words,
tails of lists are shared.

For these reasons, adding elements to and extracting elements from the
beginning of a list is much cheaper and faster than working with the
end of the list.

In some sense, lists are similar to trains. Let's look at an illustration
of a two-element list:

              . . . . . o o o o o
  _________    _________   ____  o
 |    y   |   |    x   |   |[]\_n][.
_|________|_o_|________|_o_|__|____)<
  oo    oo     oo    oo     oo 00-oo\_

     y      :      x     :  []

You can see that adding new elements (railway carriages) to the left
is easy: you just need to connect them to the last element in the
chain.

                            . . . . . o o o o o
   _________    _________    _________   ____  o
  |    z   |   |    y   |   |    x   |   |[]\_n][.
 _|________|_o_|________|_o_|________|_o_|__|____)<
   oo    oo     oo    oo     oo    oo     oo 00-oo\_

       z     :      y      :      x     :  []

But imagine how much difficult it would be to add new carriages to the right?

                            . . . . . o o o o o
   _________    _________    _________   ____  o
  |    y   |   |    x   |   |    z   |   |[]\_n][.
 _|________|_o_|________|_o_|________|_o_|__|____)<
   oo    oo     oo    oo     oo    oo     oo 00-oo\_

       y     :      x      :      z     :  []

You can't simply attach a new carriage anymore. You need to detach the
locomotive, maybe move trains around the railway a bit for the proper
position, and only then attach everything back again. The same thing
with adding elements to the end of the list — it is a slow and costly
process.

-}

{- |
=⚔️= Task 3

Let's write our first function to process lists in Haskell! Your first
implementation task is to write a function that returns all elements
of a list between two given positions inclusive (starting from zero).

Remember that each function returns a new list.

>>> subList 3 5 [1 .. 10]
[4,5,6]
>>> subList 3 0 [True, False, False, True, False]
[]

♫ NOTE: When implementing, think about various corner cases. You
  should return an empty list when given numbers that are negative.

And also don't forget to check the 'Data.List' module. It is full of
yummy functions.

Don't forget that you can load this module in GHCi to call functions
from it!

ghci> :l src/Chapter2.hs
-}
subList :: Int -> Int -> [a] -> [a]
subList a b xs
  | a < 0 || b < 0 || a > b = []
  | otherwise = drop a (take (b+1) xs)

{- |
=⚔️= Task 4

Implement a function that returns only the first half of a given list.

>>> firstHalf [3, 4, 1, 2]
[3,4]
>>> firstHalf "bca"
"b"
-}
-- PUT THE FUNCTION TYPE IN HERE
firstHalf :: [a] -> [a]
firstHalf l = take (length l `div` 2) l


{- |
=🛡= Pattern matching

One of the coolest and most powerful features of Functional
Programming is __pattern matching__. This feature allows you to match
on different values of a type and produce results based on
patterns. The syntax of using pattern matching is similar to defining
ordinary functions, but instead of using variable names, you use the
values.

For example, the "not" function that returns "the other" boolean is
implemented like this:

@
not :: Bool -> Bool
not True = False
not False = True
@

To perform pattern-matching, repeat a function name as many times as
many patterns you want to cover. The cool thing about Haskell is that
the compiler warns you if you forget to cover some cases. So you
always can be sure that your patterns are exhaustive!

Note that you can pattern match on a variable too! Variable is like a
pattern that matches any value and gives it a name. You can think of
variables in function definitions as special cases of pattern
matching.

You can pattern match on numbers as well! For example, if you want to
write a function that checks whether the given number is zero, you can
write it in the following way:

@
isZero :: Int -> Bool
isZero 0 = True
isZero n = False
@

Instead of "isZero n = False" you can write "isZero _ = False". The
symbol "_" (underscore) is called __hole__, and it is used when we
don't care about the value of a variable. It is like a pattern that
always matches (the same as a variable), but we don't use its value.

👩‍🔬 Unlike 'switch' and 'case' in other languages, that try to go
  through each switch and perform all actions in there until it reaches
  the breakpoint, pattern matching on function parameters in Haskell
  always returns only a single expression for a single branch. You can
  think of this process as trying to match all patterns from the first
  one to the last one and returning the expression on the right side
  of "=" only for the pattern that matches first. This is a helpful
  thing to keep in mind, especially when you have overlapping patterns.
  Also note that, if no pattern matches the value, the function fails
  at runtime.


In addition to pattern matching in the function definition, you can
also use the __case-of__ expression. With case-of, you specify the
patterns to match and expressions to return depending on a pattern
inside the function body. The main difference between 'case-of' and
top-level pattern matching is the fact that 'case' uses arrows (->)
instead of "=" for branch results. The "case" is often helpful when
function names are long, or pattern-matching on functions is awkward.

To understand case-of, let's look at a function that takes two numbers
and a symbol, representing a math operation on these symbols.

@
evalOperation :: Char -> Int -> Int -> Int
evalOperation op x y = case op of
    '+' -> x + y
    '-' -> x - y
    '*' -> x * y
    '/' -> div x y
    _ -> 0
@

♫ NOTE: Each branch with a pattern should have the same alignment!
  Remember that Haskell is a _layout-sensitive_ language. Also, note
  that in the last line, "_" goes directly under the single quote to
  have the same indentation 🔍. You can try copying the function and
  change the indentation to see the parser error (which is not that
  clever to identify the indentation errors).

Since we are talking about lists in this chapter, let's see how we can
use pattern-matching on them! It turns out, pattern matching on lists
is an effective and inevitable technique.

We can pattern-match on list literals directly:

@
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

sumThree :: [Int] -> Int
sumThree [x, y, z] = x + y + z
sumThree _ = 0

onlyTwoElements :: [a] -> Bool
onlyTwoElements [_, _] = True
onlyTwoElements _ = False
@

Remember the ":" operator to add elements at the beginning of a list?
Turns out, in case of lists this operator can be used for pattern
matching as well! Isn't this cool? For example:

@
-- return the first element of the list or default if the list is empty
headOrDef :: a -> [a] -> a
headOrDef def [] = def
headOrDef _ (x:_) = x

-- check if the first list element is zero
firstIsZero :: [Int] -> Bool
firstIsZero (0:_) = True
firstIsZero _ = False

-- check that a list has at least two elements
atLeastTwo :: [a] -> Bool
atLeastTwo (_ : _ : _) = True
atLeastTwo _ = False
@

When matching on the ":" pattern, the first element of the list goes
to the left side of ':' and the tail of the list goes to the right
side. You can have even nested patterns (as in the last example
above). In other words, when writing a pattern like "(x:y:xs)", it is
the same as writing "(x:(y:xs))".

♫ NOTE: Often, pattern matching can be replaced with conditional
  checks (if-then-else, guards) and vice versa. In some cases
  pattern-matching can be more efficient; in other cases, it can produce
  cleaner code or even more maintainable code due to pattern coverage
  checker from the Haskell compiler.
-}

{- |
=⚔️= Task 5

Implement a function that checks whether the third element of a list
is the number 42.

>>> isThird42 [1, 2, 42, 10]
True
>>> isThird42 [42, 42, 0, 42]
False
-}
isThird42 :: [Int] -> Bool
isThird42 (_ : _ : 42 :_) = True
isThird42 _ = False


{- |
=🛡= Recursion

Often, when writing in a functional style, you end up implementing
__recursive__ functions. Recursive functions are nothing more than
calling the function itself from the body of the same function.

Of course, you need some stopping conditions to exit the function
eventually, and you need to think carefully whether your function ever
reaches the stop condition. However, having pattern-matching in our
arsenal of skills significantly increases our chances of writing
correct functions. Nevertheless, you should think mindfully on how
your recursive function behaves on different corner-cases.

A simple recursive function can divide a number by 2 until it reaches
zero:

@
divToZero :: Int -> Int
divToZero 0 = 0
divToZero n = divToZero (div n 2)
@

But as you can see, the function is not that helpful per se. Often you
implement a helper function with some accumulator in order to collect
some information during recursive calls.

For example, we can patch the previous function to count the number of
steps we need to take in order to reduce the number to zero.

🤔 Blitz question: can you guess what this number represents?

@
divToZero :: Int -> Int
divToZero n = go 0 n
  where
    go :: Int -> Int -> Int
    go acc 0 = acc
    go acc n = go (acc + 1) (div n 2)
@

👩‍🔬 The pattern of having a recursive helper function is called "Recursive go":

  * https://kowainik.github.io/posts/haskell-mini-patterns#recursive-go

One of the most useful capabilities of pattern matching on lists is
the ability to implement recursive functions with them as well!

♫ NOTE: The canonical naming scheme for such patterns is `(x:xs)`
  where x is the first element of the list, and xs — rest of the list
  (which is a list as well that even could be empty).

@
-- list length
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- add 10 to every number of a list
addEvery10 :: [Int] -> [Int]
addEvery10 [] = []
addEvery10 (x:xs) = (x + 10) : addEvery10 xs
@

When writing such functions, we usually handle two cases: empty list
and non-empty list (list with at least one element in the beginning)
and we decide what to do in each case.

Most of the time, the case with a non-empty list uses the recursive
call to the function itself.

An example of a standard Haskell function is 'concat' that takes a
list of lists and returns a flat list, appending all intermediate
lists:

@
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
@

And it works like this:

>>> concat [[2, 1, 3], [1, 2, 3, 4], [0, 5]]
[2,1,3,1,2,3,4,0,5]
-}


{- |
=⚔️= Task 6

Implement a function that duplicates each element of the list

🕯 HINT: Use recursion and pattern matching on the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "abac"
"aabbaacc"

-}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x, x] ++ duplicate xs


{- |
=⚔️= Task 7
Write a function that takes elements of a list only in even positions.

🕯 HINT: You need to write a recursive function that pattern matches
  on the list structure. Your function will have several cases and
  probably needs to use nested pattern matching on lists of size at
  least 2. Alternatively, you can use the "Recursive go" pattern.

>>> takeEven [2, 1, 3, 5, 4]
[2,3,4]
-}
takeEven :: [a] -> [a]
takeEven [] = []
takeEven [x] = [x]
takeEven [x, _] = [x]
takeEven (x:_:xs) = x:takeEven xs

{- |
=🛡= Higher-order functions

Some functions can take other functions as arguments. Such functions
are called __higher-order functions__ (HOFs). Check the types of some
common HOF list functions:

>>> :t filter
filter :: (a -> Bool) -> [a] -> [a]
>>> :t map
map :: (a -> b) -> [a] -> [b]

And few usage examples of those functions:

>>> filter even [1..10]  -- keep only even elements in the list
[2,4,6,8,10]
>>> map not [True, False, True]  -- maps the 'not' function over each element of the given list
[False,True,False]

Having HOFs in your language means that functions can be treated in
the same way as any other values and expressions:

 ✲ You can pass functions as arguments
 ✲ You can return functions as results
 ✲ You can compose functions easily to create new functions
 ✲ You can have lists of functions
 ✲ And much more!

The ability to create __lambdas__ (or anonymous functions) nicely
complements the concept of HOF. For example, we can easily add
number 3 to each element of the list by introducing a lambda function:

>>> map (\x -> x + 3) [0..5]
[3,4,5,6,7,8]

The syntax of lambda functions is somewhat similar to normal ones,
except for you don't need to think about its name, which is
awesome. To establish the start of the lambda function, you should
write "\" which is a bit similar to the lambda symbol — λ. Then you
specify space-separated arguments. Instead of the "=" in the ordinary
function body, you should write "->" and then you can use these
arguments and all variables in scope inside the lambda-body.

These are equal:

@
foo a b = a + b
--and
\a b -> a + b
@

What's even cooler is the ability to __apply functions partially__
This means that you can provide only some arguments to a function and
treat the result as a function itself! You already know the 'div'
function: it takes two numbers and returns the result of the integral
division of those numbers. But if we apply 'div' to a number 10
partially, we will get a new function that takes only one number and
returns the result of dividing 10 by that number. You can check the
difference by inspecting the types of corresponding expressions in
GHCi:

>>> :t +d div
div :: Integer -> Integer -> Integer
>>> :t +d div 10
div 10 :: Integer -> Integer


This fact can be used to pass partial applications of some functions
directly to other functions.

>>> map (div 10) [1 .. 10]
[10,5,3,2,2,1,1,1,1,1]

You can apply operators partially too!

>>> filter (< 3) [2, 1, 3, 4, 0, 5]
[2,1,0]
>>> map (* 2) [1..5]
[2,4,6,8,10]

The implementation of the "map" function is pretty straightforward if
you are already familiar with function application, recursion and
pattern matching.

@
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
@

Now you can see that there is nothing magic in HOFs in the end!
-}

{- |
=⚔️= Task 8

Implement a function that repeats each element as many times as the
value of the element itself

>>> smartReplicate [3, 1, 2]
[3,3,3,1,2,2]

🕯 HINT: Use combination of 'map' and 'replicate'
-}
smartReplicate :: [Int] -> [Int]
smartReplicate l = concat(map (\x -> replicate x x) l)

{- |
=⚔️= Task 9

Implement a function that takes a number, a list of lists and returns
the list with only those lists that contain a passed element.

>>> contains 3 [[1, 2, 3, 4, 5], [2, 0], [3, 4]]
[[1,2,3,4,5],[3,4]]

🕯 HINT: Use the 'elem' function to check whether an element belongs to a list
-}
contains :: Int -> [[Int]] -> [[Int]]
contains x = filter (x `elem`)


{- |
=🛡= Eta-reduction

Another consequence of the HOFs and partial application is
__eta-reduction__. This term is used to call the simplification of
functions over their arguments. Specifically, if we have `foo x = bar
10 x`, this precisely means that `foo` is a partially applied `bar
10`. And we can write it like `foo = bar 10`.

This concept can be used to write functions as well.

For example,

@
nextInt :: Int -> Int
nextInt n = add 1 n
@

Could be written with the eta-reduced form:

@
nextInt :: Int -> Int
nextInt = add 1
@

♫ NOTE: See that the initial type of the function is not changed and
  it works absolutely the same. We just can skip the last argument and
  amend its usage in the function body.
-}

{- |
=⚔️= Task 10

Let's now try to eta-reduce some of the functions and ensure that we
mastered the skill of eta-reducing.
-}
divideTenBy :: Int -> Int
divideTenBy = div 10

listElementsLessThan :: Ord a => a -> [a] -> [a]
listElementsLessThan x = filter (< x)

-- Can you eta-reduce this one???
pairMul :: Num c => [c] -> [c] -> [c]
pairMul = zipWith (*)

{- |
=🛡= Lazy evaluation

Another unique Haskell feature is __lazy evaluation__. Haskell is lazy
by default, which means that it doesn't evaluate expressions when not
needed. The lazy evaluation has many benefits: avoid doing redundant
work, provide more composable interfaces. And in this section, we will
focus on Haskell's ability to create infinite data structures and work
with them!

For instance, the Haskell standard library has the 'repeat' function
that returns an infinite list created from a given element. Of course,
we can't print an infinite list to our terminal; it will take an
infinite amount of time! But we can work with parts of it due to lazy
evaluation:

>>> take 5 (repeat 0)
[0,0,0,0,0]

Another useful construction is an infinite list of all numbers!

>>> take 4 [0 ..]
[0,1,2,3]

Isn't this awesome?! Now we can unleash the real power of the
Infinity Stone!

♫ NOTE: Infinite lists bring great power, but with great power comes
  great responsibility. Functions like 'length' hang when called on
  infinite lists. So make sure you think about such corner cases in the
  implementations of your functions if you expect them to work on the
  infinite lists.
-}

{- |
=⚔️= Task 11

Rotating a list by a single element is the process of moving the first
element of the list to the end.

Implement a function to rotate a given finite list by N elements. Try
to do it more efficiently than rotating by a single element N times.

On invalid input (negative rotation coefficient) it should return an empty
list.

>>> rotate 1 [1,2,3,4]
[2,3,4,1]
>>> rotate 3 [1,2,3,4]
[4,1,2,3]

🕯 HINT: Use the 'cycle' function
-}
rotate :: Int -> [a] -> [a]
rotate x l
  | x < 0 = []
  | otherwise = take (length l) (drop x (cycle l))

{- |
=💣= Task 12*

Now you should be ready for the final boss at the end of this chapter!
To defeat the boss, implement the reversing function that takes a list
and reverses it.

>>> rewind [1 .. 5]
[5,4,3,2,1]

♫ NOTE: The Haskell standard library already provides the "reverse"
  function, but in this task, you need to implement it manually. No
  cheating!
-}
rewind :: [a] -> [a]
rewind [] = []
rewind (x:xs) = rewind xs ++ [x] 


{-
You did it! Now it is time to open pull request with your changes
and summon @vrom911 and @chshersh for the review!
-}
