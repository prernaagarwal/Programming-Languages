Don't worry about this line.
(It sets an option in the interpreter that lets us use a certain syntax.)

> {-# language ExplicitForAll #-}

Real homework text starts here.

> module HW3 where

So far in class, we've been studying _untyped_ lambda calculus.

Haskell is built on top of a similar calculus, but it's a _typed_ lambda calculus.


The syntax is very similar. In class, the lambda syntax we've been using is

  (\x. e)

(where the "\" represents a lower-case Greek lambda).


In Haskell, we use an arrow instead of a dot.

  (\x -> e)

We call this "lambda syntax" in Haskell.


Here are some numeric functions written in lambda syntax:

> always0 :: Int -> Int
> always0 = \x -> 0

> plus1 :: Int -> Int
> plus1 = \x -> x + 1

> two :: Int
> two = plus1 1

> plus :: Int -> Int -> Int
> plus = \x -> \y -> x + y

> plus2 :: Int -> Int
> plus2 = plus 2

This isn't how we'd usually write these functions in Haskell code, but the
point is that Haskell is basically lambda calculus with a bunch of extensions,
so we can use a restricted subset of Haskell to study lambda calculus.

Arrows are right-associative and have low precedence, so the definition of
"plus" is equivalent to

  plus :: Int -> (Int -> Int)
  plus = \x -> (\y -> (x + y))

Review the lambda calculus material if lambda notation is confusing -
all the same principles apply, just remember that the lambda character
is "\" in Haskell and the dot is "->" in Haskell.


Remember to USE GHCi A LOT while you're reading and working through this file.
It can be helpful to practice understanding Haskell by working out what you
expect the result of evaluating some expression to be and then using GHCi to
check your answer. Refer to the material in the Haskell folder on D2L if you
need more review on this stuff.


Remember also that GHCi is really stupid about printing functions: if you enter

  *HW3> plus2

at the interpreter, you'll get a horribly unhelpful error like

  <interactive>:31:1: error:
      • No instance for (Show (Int -> Int)) arising from a use of ‘print’
          (maybe you haven't applied a function to enough arguments?)
      • In a stmt of an interactive GHCi command: print it

There's no great answer to the question of how to print a function, so GHCi
takes the worst of all options and just dies if you try to.

You shouldn't usually read this as an error - what it's telling you is that the
expression you entered has type "Int -> Int".

If you enter an expression with a non-function type, you should see the result
of evaluating the expression.

  *HW3> plus2 3
  5

If you get other confusing errors in GHCi, please ask about them, preferably
in the course Slack channel - you're probably not the only one confused. (Don't
post any of your answer code in the public channel, though.)



4. (1 point each)

For each of the following functions, give a definition in lambda syntax.
An English specification is provided for each function.


a) firstInt should take in two Int arguments and return the first one.

> firstInt :: Int -> Int -> Int
> firstInt = \x -> \y -> x


b) addIf should take in two Int arguments and a Bool argument and return the
   sum of the two Ints if the Bool is true, or 0 if the Bool is false.
   (You'll probably need to use if/then/else syntax.)

> addIf :: Int -> Int -> Bool -> Int
> addIf = \x -> \y -> \b -> if b then x + y else 0



A _tuple_ is a pair of values - like a struct with two fields. In Haskell, the
syntax for a tuple (in both types and expressions) is "(a, b)".

> pairOfTwos :: (Int, Int)
> pairOfTwos = (2, 2)

> threeTwos :: (Int, (Int, Int))
> threeTwos = (2, (2, 2))

> twoAndTrue :: (Int, Bool)
> twoAndTrue = (2, True)


We can _destruct_ a value of a tuple type with let/in syntax.

When "c" is a tuple and we write

  let (a, b) = c in d

we're giving the names "a" and "b" to the first and second elements of "c", and
we can use those names in the expression "d".

> addPair :: (Int, Int) -> Int
> addPair = \x -> let (x1, x2) = x in (x1 + x2)

> swapInts :: (Int, Int) -> (Int, Int)
> swapInts = \x -> let (x1, x2) = x in (x2, x1)

When splitting expressions across multiple lines in Haskell, the only principle
to remember is that the next line will be considered part of the current line
if it's indented more than the current line.

> swapInts' :: (Int, Int) -> (Int, Int)
> swapInts' =
>   \x ->
>     let (x1, x2) = x in
>       (x2, x1)



5. (2 points each)

For each of the following functions, give a definition in lambda syntax.
An English specification is provided for each function.


a) add4 should take a pair of pairs of Ints and add all of them together.

> add4 :: ((Int, Int), (Int, Int)) -> Int
> add4 =
>   \x ->
>     let (x1, x2) = x in
>       let (x11, x12) = x1 in
>         let (x21, x22) = x2 in
>           x11 + x12 + x21 + x22


b) plusOrMinus should take a pair of a Bool and an Int and add 1 to the Int if
   the Bool is true, or subtract 1 if the Bool is false.

> plusOrMinus :: (Bool, Int) -> Int
> plusOrMinus = \x -> let (x1, x2) = x in if x1 then x2 + 1 else x2 - 1



A _polymorphic_ function is one that can act on multiple different types of input.

The simplest polymorphic function is the _identity_ function, which takes in an
argument and immediately returns it.

> identity :: forall a. a -> a
> identity = \x -> x

The syntax "forall a. (a -> a)" says that this is a function from an argument
of type "a" to a result of type "a", for any type "a".

"forall" is right-associative, so the type of "identity" is equivalent to

  identity :: forall a. (a -> a)

> three :: Int
> three = identity 3

> true :: Bool
> true = identity True

Note that the "forall" construct only exists at the type level - when we write
the function call "identity 3", Haskell knows that "a" must be "Int", so we
don't have to tell it that explicitly.


A classic function involving tuples and polymorphism is the _swap_ function.
"swap" reverses the order of two things in a tuple.

> swap :: forall a. forall b. (a, b) -> (b, a)
> swap = \x -> let (x1, x2) = x in (x2, x1)

Note that "swap" works on tuples of any two types.



6. (2 points each)

For each of the following functions, give _any_ valid definition in lambda syntax.
Specifications for each function are intentionally left out.

(Make sure your definitions pass the typechecker - i.e. make sure your file compiles.)

Below each function, explain in English what your function does.

For an interesting extra exercise - not worth any homework points - try to
come up with more than one meaningfully different definition of each
function, and see if you can come up with an explanation for what you
discover in the process.


a)

> q6a :: forall a. forall b. (a, b) -> b
> q6a = \x -> let (x1, x2) = x in x2


b)

> q6b :: forall a. forall b. forall c. (a, (b, c)) -> ((a, b), c)
> q6b =
>   \x ->
>     let (x1, x2) = x in
>       let (x21, x22) = x2 in
>         ((x1, x21), x22)



There are _zero_ valid definitions of a function with this type.
("undefined" doesn't count as a valid definition in this context, nor does a
definition that never terminates.)

> empty :: forall a. forall b. a -> b
> empty = undefined

This is because the type says:
  
  "empty" is a function from ANY TYPE to ANY OTHER TYPE.

We don't know anything about "a" and "b" other than that they're types, and
that they might be different types. We have no way to check whether they're
the same or different, and if they're different, we have no way to give an
expression of a type that we don't know anything at all about.



7. (optional - up to 5 points extra credit)

Are there any valid (terminating) definitions of this function? Give a
definition or explain in English why it's not possible to give one.

The Week 7 lectures on proof theory contain some hints to one correct answer,
although it might not be clear yet how to apply the information in there to
this problem. (It's also not the simplest correct answer.)


> q7 :: forall a. forall b. ((a -> b) -> a) -> a
> q7 = undefined

  It's not possible.

  A short but kind of obscure answer is that this is the equivalent of Peirce's
  law in logic, which is not provable in intuitionistic logic, and terminating
  Haskell expressions correspond to intuitionistic proofs.

  More straightforwardly, the type asks us to build a function from "(a -> b) -> a"
  to "a". Since we know nothing about "a", the only way we can produce a value
  of it is to call the function we're given as input. This means a definition
  of "q7" must have the form

    \f -> f g

  for some function "g" of type "a -> b", and if we can come up with a
  definition for "g", we have a definition for the function.
  
  However, there's no valid definition of "g" for the same reason there's no
  valid definition of "empty": knowing nothing about the types "a" and "b", we
  have no way to construct an expression of type "b" for "g" to return.
