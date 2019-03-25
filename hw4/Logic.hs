{-# options -fwarn-incomplete-patterns #-}
{-# language OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import System.Environment

type Name = String

data Prop
  = Atom Name
  | T
  | And Prop Prop
  | Or  Prop Prop
  | Imp Prop Prop
  deriving (Show, Eq)

instance IsString Prop where fromString = Atom

infix 5 ~>
(~>) = Imp

infix 6 \/
(\/) = Or

infix 7 /\
(/\) = And

pretty :: Prop -> String
pretty (Atom x) = x
pretty T = "T"
pretty (And p1 p2) = "(" ++ pretty p1 ++ " /\\ " ++ pretty p2 ++ ")"
pretty (Or  p1 p2) = "(" ++ pretty p1 ++ " \\/ " ++ pretty p2 ++ ")"
pretty (Imp p1 p2) = "(" ++ pretty p1 ++ " -> "  ++ pretty p2 ++ ")"

data ProofTree
  = Hyp Int
  | T_I
  | And_I ProofTree ProofTree | And_E ProofTree ProofTree
  | OrL_I ProofTree Prop | OrR_I Prop ProofTree | Or_E ProofTree ProofTree ProofTree
  | Imp_I Prop ProofTree | Imp_E ProofTree ProofTree
  deriving (Eq, Show)

type Ctx = [Prop]


validate :: Ctx -> ProofTree -> Maybe Prop

validate g (Hyp i) =
  if i < length g then
    Just (g !! i)
  else
    Nothing

validate g T_I =
  Just T

validate g (And_I p1 p2) =
  case (validate g p1, validate g p2) of
    (Just a1, Just a2) -> Just (And a1 a2)
    _ -> Nothing

validate g (And_E p q) =
  case validate g p of
    Just (And a1 a2) -> validate (a1:a2:g) q
    _ -> Nothing

validate g (OrL_I p1 a2) =
  case validate g p1 of
    Just a1 -> Just (Or a1 a2)
    _ -> Nothing

validate g (OrR_I a1 p2) =
  case validate g p2 of
    Just a2 -> Just (Or a1 a2)
    _ -> Nothing

validate g (Or_E p q1 q2) =
  case validate g p of
    Just (Or a1 a2) -> case (validate (a1:g) q1, validate (a2:g) q2) of
      (Just b, Just b') | b == b' -> Just b
      _ -> Nothing
    _ -> Nothing

validate g (Imp_I a1 p) =
  case validate (a1:g) p of
    Just a2 -> Just (Imp a1 a2)
    _ -> Nothing

validate g (Imp_E p q) =
  case (validate g p, validate g q) of
    (Just (Imp a1 a2), Just a1') ->
      if a1 == a1' then
        Just a2
      else
        Nothing
    _ -> Nothing

sub :: (Int -> ProofTree) -> ProofTree -> ProofTree
sub f (Hyp i) = f i
sub f T_I = T_I
sub f (And_I p1 p2) = And_I (sub f p1) (sub f p2)
sub f (And_E p1 p2) = And_I (sub f p1) (sub (f . succ . succ) p2)
sub f (OrL_I p1 a2) = OrL_I (sub f p1) a2
sub f (OrR_I a1 p2) = OrR_I a1 (sub f p2)
sub f (Or_E p1 p2 p3) = Or_E (sub f p1) (sub (f . succ) p2) (sub (f . succ) p3)
sub f (Imp_I a1 p2) = Imp_I a1 (sub (f . succ) p2)
sub f (Imp_E p1 p2) = Imp_E (sub f p1) (sub f p2)

ren = sub Hyp

beta :: ProofTree -> Int -> ProofTree
beta p 0 = p
beta p n = Hyp (n-1)

beta2 :: ProofTree -> ProofTree -> Int -> ProofTree
beta2 p1 p2 0 = p1
beta2 p1 p2 1 = p2
beta2 p1 p2 n = Hyp (n-2)

step :: ProofTree -> ProofTree
step (And_E (And_I p1 p2) p3) = sub (beta2 p1 p2) p3
step (Or_E (OrL_I p1 a2) p3 p4) = sub (beta p1) p3
step (Or_E (OrR_I a1 p2) p3 p4) = sub (beta p2) p4
step (Imp_E (Imp_I a1 p2) p3) = sub (beta p3) p2
step (Hyp i) = Hyp i
step T_I = T_I
step (And_I p1 p2) = And_I (step p1) (step p2)
step (OrL_I p1 a2) = OrL_I (step p1) a2
step (OrR_I a1 p2) = OrR_I a1 (step p2)
step (Imp_I a1 p2) = Imp_I a1 p2
step (And_E p1 p2) = And_E (step p1) p2
step (Or_E p1 p2 p3) = Or_E (step p1) p2 p3
step (Imp_E p1 p2) = Imp_E (step p1) p2

normalize :: ProofTree -> ProofTree
normalize p =
  let p' = step p in
    if p == p' then
      p
    else
      step p'

props :: Int -> [Prop]
props 0 = []
props n = concat $ transpose
  [ [Atom x | x <- ["p", "q", "r"]]
  , props (n-1)
  , [And a1 a2 | a1 <- props (n-1), a2 <- props (n-1)]
  , [Imp a1 a2 | a1 <- props (n-1), a2 <- props (n-1)]
  , [Or a1 a2 | a1 <- props (n-1), a2 <- props (n-1)]
  ]

proofTrees' :: Int -> Int -> [ProofTree]
proofTrees' h 0 = []
proofTrees' h n = concat $ transpose
  [ [Hyp i | i <- [0..h-1]]
  , proofTrees' h (n-1)
  , [And_E p1 p2 | p1 <- proofTrees' h (n-1), p2 <- proofTrees' (2 + h) (n-1)]
  , [Imp_E p1 p2 | p1 <- proofTrees' h (n-1), p2 <- proofTrees' h (n-1)]
  , [Or_E p1 p2 p3 | p1 <- proofTrees' h (n-1), p2 <- proofTrees' (1 + h) (n-1), p3 <- proofTrees' (1 + h) (n-1)]
  , [And_I p1 p2 | p1 <- proofTrees' h (n-1), p2 <- proofTrees' h (n-1)]
  , [OrL_I p1 a2 | p1 <- proofTrees' h (n-1), a2 <- props (n-1)]
  , [OrR_I a1 p2 | a1 <- props (n-1), p2 <- proofTrees' h (n-1)]
  , [Imp_I a1 p2 | a1 <- props (n-1), p2 <- proofTrees' (1 + h) (n-1)]
  ]

proofTrees = proofTrees' 0

size :: ProofTree -> Int
size (Hyp i)          = 1
size T_I              = 0
size (And_I p1 p2)    = 2 + size p1 + size p2
size (And_E p1 p2)    = 3 + size p1 + size p2
size (OrL_I p1 a2)    = 2 + size p1
size (OrR_I a1 p2)    = 2 + size p2
size (Or_E  p1 p2 p3) = 6 + size p1 + size p2 + size p3
size (Imp_I a1 p2)    = 4 + size p2
size (Imp_E p1 p2)    = 3 + size p1 + size p2

bigEnough :: ProofTree -> Bool
bigEnough p = size p > 8

tooBig :: ProofTree -> Bool
tooBig p = size p > 12

tooUninteresting :: Prop -> Bool
tooUninteresting (And p1 p2) = p1 == p2 || tooUninteresting p1 || tooUninteresting p2
tooUninteresting (Or p1 p2) = p1 == p2 || tooUninteresting p1 || tooUninteresting p2
tooUninteresting (Imp p1 (Or p2 p3)) = p1 == p2 || p1 == p3 || p1 == Or p2 p3 || tooUninteresting p1 || tooUninteresting p2 || tooUninteresting p3
tooUninteresting (Imp p1 (Imp p2 p3)) = p1 == p2 || p1 == Imp p2 p3 || tooUninteresting p1 || tooUninteresting (Imp p2 p3)
tooUninteresting (Imp p1 p2) = p1 == p2 || tooUninteresting p1 || tooUninteresting p2
tooUninteresting _ = False

assignmentProps :: Int -> [Prop]
assignmentProps n =
  nub
    [ a
    | p <- proofTrees n
    , a <- maybeToList (validate [] p)
    , let p' = normalize p
    , bigEnough p'
    , not (tooBig p')
    , not (tooUninteresting a)
    ]

main :: IO ()
main = do
  args <- getArgs
  if args == [] then
    putStrLn "run with at least one argument"
  else
    let
      input = concat args
      nums = map ord input
      top = 512
      ps = take top (drop top (assignmentProps 5))
    in
      mapM_ (putStrLn . pretty . (ps !!))
        [ sum nums `mod` top
        , (sum nums ^ 2) `mod` top
        , (sum nums ^ 3) `mod` top
        , product nums `mod` top
        , foldl (-) 0 nums `mod` top
        ]

-- A /\ B -> B /\ A
andComm :: ProofTree
andComm =
  Imp_I ("A" /\ "B")
    (And_I
      (And_E
        (Hyp 0)   -- g = [A /\ B]
        (Hyp 1))  -- g = [A, B]
      (And_E
        (Hyp 0)   -- g = [A /\ B]
        (Hyp 0))) -- g = [A, B]


-- A /\ (B /\ C) -> (A /\ B) /\ C
andAssoc :: ProofTree
andAssoc =
  Imp_I ("A" /\ ("B" /\ "C"))
    (And_I
      (And_I
        (And_E
          (Hyp 0)      -- g = [A /\ (B /\ C)]
          (Hyp 0))     -- g = [A, B /\ C]
        (And_E
          (Hyp 0)      -- g = [A /\ (B /\ C)]
          (And_E
            (Hyp 1)    -- g = [A, B /\ C]
            (Hyp 0)))) -- g = [B, C]
      (And_E
        (Hyp 0)        -- g = [A /\ (B /\ C)]
        (And_E
          (Hyp 1)      -- g = [A, B /\ C]
          (Hyp 1))))   -- g = [B, C]
