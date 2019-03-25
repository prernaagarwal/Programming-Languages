> module HW1 where

This document is a literate Haskell file (because of the .lhs extension): lines starting with the > character are executable Haskell code, and all other lines in the file are treated as comments. The line above is the beginning of this program.

First things first: if you're working on your own computer and have never worked with Haskell before, you'll need to install the Haskell platform. Go to https://www.haskell.org/platform and follow the instructions for your operating system; if it gives you an option between the "core" and "full" versions, choose "core". (The full version will also work for this course, but comes with a lot of extra libraries that we won't need.) If you're using the department Linux machines, they already have GHCi installed, so you shouldn't have to do anything to get set up.

We'll be working with literate Haskell files in all of the homework assignments in this course, so let's first go over the workflow you should be using. Instead of compiling a program to a binary file and then running that file, you'll interact directly with the Haskell file in GHCi, which is basically a special kind of command line designed for this purpose. It's usually easiest to work with the program open in a text editor in one window and open in GHCi in another window, side by side.

Open this file in your favorite text editor, and then run GHCi from your command line.

  ghci hw1.lhs

You should see some output that indicates GHCi has started up (don't worry if your version number is different).

  GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
  [1 of 1] Compiling HW1              ( hw1.lhs, interpreted )
  Ok, modules loaded: HW1.
  *HW1> 

You can enter a Haskell expression here and see what it evaluates to. Follow along with these prompts in your command line to make sure GHCi is working properly: input the part after *HW1> and hit Enter, and you should see the next line as output.
  
  *HW1> 1 + 1
  2

  *HW1> 3.1 * 5
  15.5

  *HW1> "he" ++ "llo"
  "hello"

  *HW1> [1, 2, 3] ++ [4, 5, 6]
  [1, 2, 3, 4, 5, 6]

Beware that function calls are written differently in Haskell than in many languages. In general, the form is a function name followed by a space and then all of the arguments separated by spaces.

  C++:     f(x)
  Haskell: f x

  C++:     f(x, y, z)
  Haskell: f x y z

  C++:     max(1 + 2, 3 + 4)
  Haskell: max (1 + 2) (3 + 4)

  C++:     max(min(1, 2), min(3, 4))
  Haskell: max (min 1 2) (min 3 4)

This is what definitions in our Haskell files will look like: one line with the type, followed by one or more lines with the definition. This definition says that flag has type Bool, and the value of flag is False.

> flag :: Bool
> flag = False

  *HW1> flag
  False

When you make a change to a file, you can reload it in GHCi with the :r command. Try this out to make sure you've got everything working correctly: change the definition of flag to be True instead of False, and then run these commands to verify that GHCi sees your change.

  *HW1> :r
  Ok, modules loaded: HW1.

  *HW1> flag
  True

This is the workflow you'll be using for all of the programming assignments in this course: edit, reload, test, repeat.

This is the Prop type introduced in the Week 1 lecture. (The "deriving Show" line tells GHCi that the Prop type should be printable.)

> data Prop
>   = TRUE | FALSE | IN String
>   | AND Prop Prop | OR Prop Prop | NOT Prop
>   deriving Show     

You can enter Prop expressions into GHCi.

  *HW1> TRUE
  TRUE

  *HW1> IN "A"
  IN "A"

  *HW1> AND TRUE FALSE
  AND TRUE FALSE

Remember that the Prop type is a type of syntax: AND TRUE FALSE doesn't evaluate to TRUE, because a Prop expression is a representation of the text of a program, not an actual program itself. We can use the evaluation function from the Week 1 slides to run a Prop program given a list of inputs. You shouldn't have to understand how this function works to finish this homework assignment, but it's a good idea to study it when you have time in order to get more familiar with Haskell.

> type Env = String -> Bool

> eval :: Prop -> Env -> Bool
> eval (AND p q) env = eval p env && eval q env
> eval (OR p q)  env = eval p env || eval q env
> eval (NOT p)   env = not (eval p env)
> eval TRUE      env = True
> eval FALSE     env = False
> eval (IN v)    env = env v

An environment is a mapping (function) from names to values. A function definition has the type of the function on one line, followed by a line for each possible input.

> env1 :: Env
> env1 "A" = True
> env1 "B" = False
> env1 "C" = True

  *HW1> env1 "A"
  True

  *HW1> env1 "B"
  False

  *HW1> eval "C"
  True

If you try to call env1 with a name that it doesn't have a case for, it'll fail with an exception.

Note that an Env isn't printable by itself in GHCi - you'll get a confusing error if you try to evaluate one.

  *HW1> env1
  <interactive>:21:1: error:
      • No instance for (Show Env) arising from a use of ‘print’
          (maybe you haven't applied a function to enough arguments?)
      • In a stmt of an interactive GHCi command: print it

There are other ways to represent environments that don't have this issue, but functions are convenient enough for the purposes of this assignment.

Now we can evaluate a Prop program in any environment we define.

  C++: (true && false)

> prop1 :: Prop
> prop1 = AND TRUE FALSE

  *HW1> eval prop1 env1
  False


  C++: A || B

> prop2 :: Prop
> prop2 = OR (IN "A") (IN "B")

  *HW1> eval prop2 env1
  True


  C++: !A && !B

> prop3 :: Prop
> prop3 = AND (NOT (IN "A")) (NOT (IN "B"))

  *HW1> eval prop3 env1
  False

4. Write a Prop program to represent each of the following Boolean-valued C++ expressions. Replace the word "undefined" with your answer in each definition. (1 point each)


  a) !(A || A)

> q4a :: Prop
> q4a = undefined


  b) !(!A && !!B)

> q4b :: Prop
> q4b = undefined


  c) (A && !B) || C

> q4c :: Prop
> q4c = undefined


ALWAYS TEST ALL OF YOUR ANSWERS IN GHCi. This assignment doesn't ask for any evidence of testing, but future assignments will. You'll lose a lot of points in this class if you ignore this warning. ALWAYS TEST YOUR CODE BEFORE YOU SUBMIT AN ASSIGNMENT.

Here are a few more environments that you can use for testing; feel free to add more.

> env2 :: Env
> env2 "A" = False
> env2 "B" = True
> env2 "C" = False

> env3 :: Env
> env3 "A" = True
> env3 "B" = True
> env3 "C" = True

> env4 :: Env
> env4 "A" = True
> env4 "B" = True
> env4 "C" = False


5. Define the environments q5a and q5b so that "eval prop5 q5a" returns False and "eval prop5 q5b" returns True. If either result is impossible, argue why it's impossible. Each question may have more than one correct answer; you just have to give one. (1 point each)

> prop5 :: Prop
> prop5 = AND (IN "A") (NOT (OR (IN "B") (AND FALSE (IN "C"))))

> q5a :: Env
> q5a "A" = undefined
> q5a "B" = undefined
> q5a "C" = undefined

> q5b :: Env
> q5b "A" = undefined
> q5b "B" = undefined
> q5b "C" = undefined
