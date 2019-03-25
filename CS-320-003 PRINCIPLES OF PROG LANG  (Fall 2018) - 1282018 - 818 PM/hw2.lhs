> module HW2 where

Here's the definition of the primitive syntax of regular expressions, from the
week 3 slides:

> data Regex
>   = Epsilon                -- empty string
>   | Sing Char              -- single character
>   | Concat Regex Regex     -- concatenation
>   | Alternate Regex Regex  -- alternation
>   | Star Regex             -- repetition (one or more)
>   deriving Show

We'll also define some more convenient infix operators, so we can write
expressions of type Regex with a slightly more readable notation.

> (.|.) :: Regex -> Regex -> Regex
> (.|.) = Alternate

> (.@.) :: Regex -> Regex -> Regex
> (.@.) = Concat

Haskell allows us to specify the associativity and precedence of custom
operators with the "infixl" (left associative) and "infixr" (right
associative) commands; the number given is the precedence, where higher
numbers bind tighter. It's conventional for the concatenation operator to bind
tighter than the alternation operator in regular expression syntax, so that e.g.

  "Sing 'a' .@. Sing 'b' .|. Sing 'c' .@. Sing 'd'"
  
parses as

  "(Sing 'a' .@. Sing 'b') .|. (Sing 'c' .@. Sing 'd')".

> infixr 2 .|.
> infixr 3 .@.


As with the "Prop" syntax definition from week 1, we can define functions that
operate on Regex values by giving a case for each constructor; here we define
a "pretty printing" function for Regex expressions. It's somewhat tricky to
build a pretty printing function that only shows parentheses when they're
strictly required, so this function conservatively wraps every binary operator
application in parentheses; it's not beautiful output, but it might be a little
easier to read than the Haskell syntax.

In the slides, we used a lower-case epsilon to represent the empty string, but
that doesn't work in some command line programs, so we'll use the # symbol to
represent the empty string in the output of this function.

> prettify :: Regex -> String
> prettify Epsilon = "#"
> prettify (Sing c) = [c]
> prettify (Concat r1 r2) = "(" ++ prettify r1 ++ prettify r2 ++ ")"
> prettify (Alternate r1 r2) = "(" ++ prettify r1 ++ "|" ++ prettify r2 ++ ")"
> prettify (Star r) = "(" ++ prettify r ++ ")*"


Now that we have a programmatic definition of the syntax of regular
expressions, we can build a function that matches strings against a regex -
i.e. a function that checks whether or not an input string is a member of the
language specified by a given regex.

The function we'll define here is actually a little more general: it checks
whether any prefix of the input string is a member of the specified language,
and returns the rest of the string (the unmatched suffix) if it finds a match.
For example, the regex "Sing 'a' .@. Sing 'b'" matched against the string
"abcd" should return a suffix of "cd".


5. (2 points)

Fill in the definitions of the Sing and Alternate cases for the "check" function:

  - "Sing c" is the regex that matches exactly the single character c.
  - "Alternate r1 r2" is the regex that matches any string that either r1 or r2 matches.

> check :: Regex -> String -> Maybe String
> check Epsilon cs = Just cs
> check (Sing c) (c':cs) = undefined
> check (Concat r1 r2) cs =
>   case check r1 cs of
>     Just cs' -> check r2 cs'
>     Nothing -> Nothing
> check (Alternate r1 r2) cs = undefined
> check (Star r) cs = 
>   case check r cs of
>     Just cs' -> check (Star r) cs'
>     Nothing -> Just cs
> check _ _ = Nothing


If your implementation of the Sing and Alternate cases of "check" are correct,
all of these tests should evaluate to True.

> regex1 :: Regex
> regex1 = Sing 'a' .@. Sing 'b' .@. Star (Sing 'b') .@. Sing 'c'

> test1_1 :: Bool
> test1_1 = check regex1 "abbbc" == Just ""

> test1_2 :: Bool
> test1_2 = check regex1 "abcd"  == Just "d"

> test1_3 :: Bool
> test1_3 = check regex1 "acb"   == Nothing

> regex2 :: Regex
> regex2 = string "ab" .|. string "cd"

> test2_1 :: Bool
> test2_1 = check regex2 "abcd" == Just "cd"

> test2_2 :: Bool
> test2_2 = check regex2 "cdab" == Just "ab"

> test2_3 :: Bool
> test2_3 = check regex2 "acd"  == Nothing


Another useful property of our regex representation is that we can generate
regular expressions as the output of functions, which lets us add new
constructs to the language of regular expressions without extending the
original definition. For example, this function constructs a regex that matches
exactly a given string. (Try calling it in the REPL with some different inputs
to get a feel for how it works.)

> string :: String -> Regex
> string [] = Epsilon
> string (c:cs) = Sing c .@. string cs

> test_string :: Bool
> test_string = check (string "abc") "abcdef" == Just "def"


6. (2 points)

We can also write functions to build regular expressions out of other regular
expressions. Our base syntax contains the Star constructor to match zero or
more repetitions of a regex; define the plus operator here using the Regex
constructors. (You shouldn't have to use pattern matching.)

> plus :: Regex -> Regex
> plus r = undefined


7. (6 points)

Write at least three more regexes and at least two tests for each that
demonstrate that the code you wrote in "check" and "plus" is working correctly.

> regex6a :: Regex
> regex6a = undefined

> test6a_1 :: Bool
> test6a_1 = undefined

> test6a_2 :: Bool
> test6a_2 = undefined


> regex6b :: Regex
> regex6b = undefined

> test6b_1 :: Bool
> test6b_1 = undefined

> test6b_2 :: Bool
> test6b_2 = undefined


> regex6c :: Regex
> regex6c = undefined

> test6c_1 :: Bool
> test6c_1 = undefined

> test6c_2 :: Bool
> test6c_2 = undefined
