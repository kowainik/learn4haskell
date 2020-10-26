{- 👋 Welcome, Brave folks!

Happy to see you here, on the way to the wonderful Functional Programming land
with Haskell! Fight the fierce Monad Dragon and save the globe from despicable
runtime exceptions!


We appreciate your curiosity and will try to provide you with all the necessary
equipment for your training before the battle in the real FP world. Learning
Functional Programming can be challenging. But we designed this training to be
beginner-friendly and helpful to everyone!

Practice your functional skills, and choose your class in the end, who you want
to become: Monad Wizard, Exceptions Crusher, Recursion Priest, Free Fighter,
Composition Warlock, and many more!

Here is how it works:

 ★ Make sure that you familiarise yourself with this repo's README in order to
   understand why and what we are trying to achieve with this.
 ★ For each Chapter, learn the material that we provide and try to solve each
   task proposed to consolidate the results.
 ★ Create a PR with solutions to as many tasks as you can after each Chapter is
   finished.
 ★ Make sure that tests for the Chapter you are working at are passing.
 ★ Receive some feedback and suggestions from us.
 ★ Merge the PR.
 ★ Go to the next Chapter.
 ★ When finished, tell us how you liked it. Share it with your friends. Support
   us on Ko-Fi or GitHub sponsorship.

== You are on __Chapter One__.

Here we will give the basic Haskell syntax knowledge and explain other common
concepts on the way. In this chapter, you are going to learn:

 ✧ Haskell language main particularities
 ✧ How to use Haskell interactively
 ✧ Expressions
 ✧ Types
 ✧ Basic language syntax: calling functions, if-then-else, defining local
   variables
 ✧ How to write your function from scratch
 ✧ Some standard Haskell functions


We are leaving a number of tasks on our path. Your goal is to solve them all and
make the test for Chapter One green.

After finishing the PR, you can choose to summon us, @vrom911 and @chshersh, to
look at your solution in order to give some advice on your code or help with
understanding and exercise solutions. This is optional; however, you can ask us
for review only if you want some feedback on your solutions.

Now, if you are ready, let's start!
-}

-- Single-line comments in Haskell start with --

{- | This tutorial uses block comments to explain various concepts and provide
task description.
-}

{- All code in Haskell is organised into modules. Each module corresponds to a
single file. Modules then can be combined in a package. But you don't need to
worry about this for now. We already created the package with module hierarchy
for you.

Each Haskell module starts with the "module <MODULE_NAME> where" line.
Modules should have the same name as the corresponding file with
the `.hs` extension.
-}
module Chapter1 where

{- |
In Haskell, we have __expressions__. Expressions can be represented by some
primitive values (numbers: 1, 100; characters: 'a', 'z'; booleans: True, False;
etc.) or by a combination of the primitive values and other expressions using
language syntax constructions (if-then-else, let-in, case-of, etc.) and various
functions (addition — (+), division — div, maximum — max, sorting — sort,
sortBy, sortOn, etc.) and variables. Functions are also expressions as well as
variables.

If an expression is a combination of other values and expressions, it can be
__evaluated__ (or reduced) to a primitive value. The evaluation process is not
immediate, since Haskell is a __lazy language__ and it won't evaluate
expressions unless really necessary. You can see the evaluation results either
by running a Haskell program or by playing with some functions in the
interactive interpreter (explained later).

Haskell is a __strongly-typed__ language, which means that each expression has
a type. Each value and function is associated with some type. You can't change
the value type. You can only pass a value to some function that will do its
work, and maybe produce a value of a different type.

Types can be _specific_ (like `Int`, `Integer`, `Double` or `Bool`) and they
always start with an uppercase letter, or _polymorphic_ (aka general) specified
through the variables – begin with the lowercase letter. The concept of
polymorphism is more sophisticated than working with concrete types, thus we
won't dive too much into it in this chapter and will work with the concrete
types for now.

Furthermore, Haskell is a __statically-typed__ language, which means that each
expression has the type known at compile-time, rather than run-time. It allows
the compiler to catch some kinds of bugs in your program early; before you
even run it.

Additionally to static typing, Haskell has __type inference__. This means that
you _don't need_ to specify the type of each expression as it is going to be
found out for each expression and subexpression by the powerful compiler.

However, you are __strongly encouraged to write top-level function type
signatures__ and provide types in different situations where you don't
immediately see what types will be inferred.
-}


 {-
Haskell is a __compiled__ language. At the illustration below, you can see the
overall picture of the process from your code to the binary of the written
program:

+-----------+      +-------------+     +-------------+
|           |      |             |     |             |
|  Parsing  +------> Compilation +-----> Executable  |
|           |      |             |     |             |
+-----------+      +-------------+     +-------------+

In comparison, such languages as Python, JavaScript, etc. are interpreted
languages. And that could be illustrated in the different workflow:

+-----------+      +----------------+     +------------+
|           |      |                |     |            |
|  Parsing  +------> Interpretation +-----> Evaluation |
|           |      |                |     |            |
+-----------+      +----------------+     +------------+

So, when working with Haskell, you first need to compile the code in order to
run it later.

However, most of the time while working with Haskell (especially when learning
Haskell), you use __GHCi__ to play with functions, see their behaviour and
explore API of external libraries. GHCi is a __Haskell REPL__
(read-eval-print-loop). It allows calling functions directly in an interactive
shell. GHCi interprets Haskell expressions and statements and prints the
evaluation result of each expression.

As you can see, Haskell supports both paradigms: you can either compile your
code and run the executable, or interpret the code and run it directly.

Assuming that you already have the Haskell compiler installed (we recommend
GHC), you can start GHCi by executing the following command in your terminal
from the root of this project.

$ ghci

> If you don't have Haskell yet, refer to the corresponding section of the
  README.

Now, you can evaluate some expressions and see their results immediately.

>>> 1 + 2
3

♫ NOTE: Each time you see a line starting with >>> in this training, it means
  that the text after >>> is supposed to be evaluated in GHCi, and the evaluation
  result should be printed on the next line if it has any. In your GHCi, you
  probably have the string "Prelude> " as your prompt. For some expressions, we
  already prefilled the result, but for others, you need to insert it on your own.
  We use lovely Haskell tools to test the results of the GHCi evaluation as well,
  so you'd better insert exact answers in order to make them pass the testing!

  You will also see lines started with "ghci>". They are also supposed to be
  evaluated in GHCi, but our testing system doesn't check their output.
  They are here just to showcase the different usages of GHCi.


GHCi can do much more than evaluating expressions. It also contains some special
commands starting with a colon. For example, to see the list of all available
commands, type ":?" in your GHCi.

ghci> :?

To quit GHCi, enter the ":q" command (short for ":quit").

ghci> :q

-}

{- |
=⚔️= Task 1

Since types play a crucial role in Haskell, we can start by exploring types of
some basic expressions. You can inspect the type of expression by using the ":t"
command in GHCi (short for ":type").

For example:

>>> :t False
False :: Bool

"::" in Haskell indicates that the type of the expression before, would be
specified after these symbols.
So, the output in this example means that 'False' has type 'Bool'.

(ﾉ◕ヮ◕)ﾉ Your first task! Use GHCi to discover types of the following
  expressions and functions:

> Try to guess first and then compare your expectations with GHCi output

>>> :t True
True :: Bool
>>> :t 'a'
'a' :: Char
>>> :t 42
42 :: Num p => p

A pair of boolean and char:
>>> :t (True, 'x')
(True, 'x') :: (Bool, Char)

Boolean negation:
>>> :t not
not :: Bool -> Bool

Boolean 'and' operator:
>>> :t (&&)
(&&) :: Bool -> Bool -> Bool

Addition of two numbers:
>>> :t (+)
(+) :: Num a => a -> a -> a

Maximum of two values:
>>> :t max
max :: Ord a => a -> a -> a

You might not understand each type at this moment, but don't worry! You've only
started your Haskell journey. Types will become your friends soon.

Primitive types in Haskell include 'Int', 'Bool', 'Double', 'Char' and many
more. You've also seen the arrow "->" which is a function. When you see "A -> B
-> C" you can think that this is a function that takes two arguments of types
"A" and "B" and returns a value of type "C".
-}

{- |
=⚔️= Task 2

After having our first look at the Haskell type system, we can do something more
exciting. Call to arms! In other words, let's call some functions.

When calling a function in Haskell, you type a name of the function first, and
then you specify space-separated function arguments. That's right. No commas, no
parenthesis. You only need to use () when grouping arguments (e.g. using other
expressions as arguments).

For example, if the function `foo` takes two arguments, the call of this
function can look like this:

ghci> foo arg1 (fun arg2)

Operators in Haskell are also functions, and you can define your own operators
as well! The important difference between operators and functions is that
functions are specified using alphanumeric symbols, and operators are specified
using "operator" symbols. For example, addition — +, cons — :, list append — ++,
diamond operator — <>. Also, by default, you call operators in the __infix__
form (operator goes __after__ the first argument), while ordinary functions are
what-called __prefix__ form (the name goes first, before all arguments).

ghci> :t add
Add :: Int -> Int -> Int
ghci> :t (+)
(+) :: Int -> Int -> Int
ghci> add 1 2
3
ghci> 1 + 2
3

♫ NOTE: in reality, the type of the + operator is the following:

>>> :t (+)
(+) :: Num a => a -> a -> a

> It may look scary to you, but we will cover all this 'Num' and "=>" later. For
  now, you can think of this as a polymorphic function — in this case, the
  operator, that can work with any numeric types, including 'Int's, 'Double's,
  etc. Or you can even pass the "+d" option to the ":t" command to see a simpler
  type. In this case, polymorphic types will default to some standard types:

ghci> :t +d (+)
(+) :: Integer -> Integer -> Integer

Get ready for the next task, brave programmer! Evaluate the following
expressions in GHCi

> As in the previous task, try to guess first and then compare your expectations
  with the GHCi output.

🕯 HINT: if you are curious, it might be interesting to explore types of
  functions and operators first. Remember this from the previous task? ;)

>>> 1 + 2
3

>>> 10 - 15
-5

>>> 10 - (-5)  -- negative constants require ()
15

>>> (3 + 5) < 10
True

>>> True && False
False

>>> 10 < 20 || 20 < 5
True

>>> 2 ^ 10  -- power
1024

>>> not False
True

>>> div 20 3  -- integral division
6

>>> mod 20 3  -- integral division remainder
2

>>> max 4 10
10

>>> min 5 (max 1 2)
2

>>> max (min 1 10) (min 5 7)
5

Because Haskell is a __statically-typed__ language, you see an error each time
you try to mix values of different types in situations where you are not
supposed to. Try evaluating the following expressions to see errors:

ghci> not 'a'
ghci> max True 'x'
ghci> 10 + True

This is a gentle way to get familiar with various error messages in Haskell.
In some cases, the error messages could be challenging to decipher and
understand their meaning. Haskell has a bad reputation for having not-so-helpful
error messages in some situations. But, of course, such a small challenge won't
stop you, right? You're a brave warrior, and you can finish all tasks despite
all obstacles! And we are always here to help and to decrypt these ancient
scripts together.
-}


{- |
=🛡= Defining a function

We have already learned how to use different functions and operators in Haskell.
Let's now check how they are defined and whether we can introduce our own.

When defining a function in Haskell, you write its type signature on the first
line, and then its body on the following line(s). The type signature should be
written immediately from the start of a line. Haskell is __indentation-__ and
__layout-sensitive__ language, so this is important to keep in mind.

For example, here is the type signature of a function that takes a 'Double' and
an 'Int', and then returns an 'Int':

@
roundSubtract :: Double -> Int -> Int
@

We have already seen the "::" sequence of symbols when practising our skills in
GHCi. Now you know that this is the syntax for specifying types in your code as
well.

The following line should be the function definition start line. You write the
function name again and give argument names in the same order as you wrote types
followed by the "=" sign. And you provide the function implementation after "=".

@
roundSubtract x y = ceiling x - y
@

^ Here x corresponds to the 'Double', and y to 'Int'.

The body of the function can be as big as you want. However, don't forget about
the indentation rules when your body exceeds the definition line.

The same function body can be written on a separate line, minding the
indentation.

@
roundSubtract x y =
    ceiling x - y
@

Putting everything together, the complete function definition looks like this:

@
roundSubtract :: Double -> Int -> Int
roundSubtract x y = ceiling x - y
@

Now you are ready for defining your own functions!
-}

{- |
In our training, for some functions types are provided for you. For others, you
need to write types manually to challenge yourself.

Don't forget the main rule:
**Always provide type signatures for top-level functions in Haskell.**
-}


{- |
=⚔️= Task 3

Below you see the function that finds a square of the sum of two integers. Your
task is to specify the type of this function.

>>> squareSum 3 4
49
-}
squareSum :: Int -> Int -> Int
squareSum x y = (x + y) * (x + y)


{- |
=⚔️= Task 4

Implement the function that takes an integer value and returns the next 'Int'.

>>> next 10
11
>>> next (-4)
-3

♫ NOTE: The current function body is defined using a special function called
  "error". Don't panic, it is not broken. 'error' is like a placeholder, that
  evaluates to an exception if you try evaluating it. And it also magically fits
  every type ｡.☆.*｡. No need to worry much about "error" here, just replace the
  function body with the proper implementation.
-}
next :: Int -> Int
next x = x + 1

{- |
After you've implemented the function (or even during the implementation), you
can run it in GHCi with your input. To do so, first, you need to load the module
with the function using the ":l" (short for ":load") command.

ghci> :l src/Chapter1.hs

After that, you can call the 'next' function as you already know how to do that.
Or any other function defined in this module! But remember, that you need to
reload the module again after you change the file's content. You can reload the
last loaded module by merely typing the ":r" command (no need to specify the
name again).

ghci> :r

A typical workflow looks like this: you load the module once using the ":l"
command, and then you should reload it using the ":r" command each time you
change it and want to check your changes.
-}

{- |
=⚔️= Task 5

Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2

🕯 HINT: use the `mod` function

♫ NOTE: You can discover possible functions to use via Hoogle:
    https://hoogle.haskell.org/

  Hoogle lets you search Haskell functions either by name or by type. You can
  enter the type you expect a function to have, and Hoogle will output relevant
  results. Or you can try to guess the function name, search for it and check
  whether it works for you!
-}
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10


{- |
=⚔️= Task 6

Implement a function, that takes two numbers and returns the one closer to zero:

>>> closestToZero 10 5
5
>>> closestToZero (-7) 3
3


🕯 HINT: You can use the 'abs' function and the __if-then-else__ Haskell syntax
  for this task.

'if-then-else' is a language construction for expression that returns only one
branch depending on the checked condition. For example:

>>> if even 10 then 0 else 1
0

The 'if-then-else' constructions must always have both __then__ and __else__
branches because it is an expression and it must always return some value.

👩‍🔬 Due to lazy evaluation in Haskell, only the expression from the branch
  satisfying the check will be returned and, therefore, evaluated.
-}
closestToZero :: Int -> Int -> Int
closestToZero x y = if abs x > abs y then y else x


{- |
=⚔️= Task 7
Write a function that returns the middle number among three given numbers.

>>> mid 3 1 2
2

🕯 HINT: When checking multiple conditions, it is more convenient to use the
  language construction called "guards" instead of multiple nested 'if-then-else'
  expressions. The syntax of guards is the following:

@
sign :: Int -> Int
sign n
    | n < 0 = (-1)
    | n == 0 = 0
    | otherwise = 1
@

You define different conditions in different branches, started by the '|'
symbol. And the functions check them from top to bottom, returning the first
value after "=" where the condition is true.

♫ NOTE: The "=" sign goes after each branch, respectively.

♫ NOTE: It is essential to have the same indentation before each branch "|"!
  Remember, that Haskell is indentation- and layout-sensitive language.

Casual reminder about adding top-level type signatures for all functions :)
-}
mid :: Int -> Int -> Int -> Int
mid x y z
    | x <= z && z <= y = z
    | y <= z && z <= x = z
    | y <= x && x <= z = x
    | z <= x && x <= y = x
    | otherwise        = y

{- |
=⚔️= Task 8

Implement a function that checks whether a given character is a vowel.

🕯 HINT: use guards

>>> isVowel 'a'
True
>>> isVowel 'x'
False
-}
isVowel :: Char -> Bool
isVowel c
  | c == 'a' = True
  | c == 'e' = True
  | c == 'i' = True
  | c == 'o' = True
  | c == 'u' = True
  | otherwise = False


{- |
== Local variables and functions

So far, we've been playing only with simple expressions and function
definitions. However, in some cases, expressions may become complicated, and it
could make sense to introduce some helper variables.

You can use the let-in construction in Haskell to define variables.
Here goes an example:

@
half :: Int -> Int
half n = let halfN = div n 2 in halfN
@

♫ NOTE: __let-in__ is also an expression! You can't just define variables; you
  also need to return some expression that may use defined variables.

The syntax for defining multiple variables requires to care about indentation
more, but there is nothing special in it as well:

@
halfAndTwice :: Int -> (Int, Int)
halfAndTwice n =
    let halfN = div n 2
        twiceN = n * 2
    in (halfN, twiceN)
@

In addition to let-in (or sometimes even alternatively to let-in) you can use
the __where__ construction to define local variables and functions.
And, again, the example:

@
pythagoras :: Double -> Double -> Double
pythagoras a b = square a + square b
  where
    square :: Double -> Double
    square x = x ^ 2
@

You can define multiple functions inside __where__!
Just remember to keep proper indentation.
-}

{- |
=⚔️= Task 9

Implement a function that returns the sum of the last two digits of a number.

>>> sumLast2 42
6
>>> sumLast2 134
7
>>> sumLast2 1
1

Try to introduce variables in this task (either with let-in or where) to avoid
specifying complex expressions.
-}
sumLast2 :: Int -> Int
sumLast2 n =
    let lastTwo = mod (abs n) 100
        (second, first) = divMod lastTwo 10
    in second + first


{- |
=💣= Task 10*

You did it! You've passed all the challenges in your first training!
Congratulations!
Now, are you ready for the boss at the end of this training???

Implement a function that returns the first digit of a given number.

>>> firstDigit 230
2
>>> firstDigit 5623
5

You need to use recursion in this task. Feel free to return to it later, if you
aren't ready for this boss yet!
-}
firstDigit :: Int -> Int
firstDigit n = go (divMod (abs n) 10)
	where
		go :: (Int, Int) -> Int
		go(0, lastD) = lastD
		go(rest, _) = go (divMod rest 10)


{-
You did it! Now it is time to open pull request with your changes
and summon @vrom911 and @chshersh for the review!
-}

{-
=📜= Additional resources

Modules: http://learnyouahaskell.com/modules
Let vs where: https://wiki.haskell.org/Let_vs._Where
Packages and modules in Haskell: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html
-}
