{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

import Data.List

------------------------------------------------------------
-- #1. Naive recursive method.
------------------------------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 = map fib [0..]

------------------------------------------------------------
-- #2. Simple O(n) solutions.
------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- or

fibs2' :: [Integer]
fibs2' = unfoldr (\(x1,x2) -> Just (x1, (x2, x1+x2))) (0,1)

-- or

fibs2'' :: [Integer]
fibs2'' = map fst . iterate (\(x1,x2) -> (x2, x1+x2)) $ (0,1)

------------------------------------------------------------
-- #3. Basic streams
------------------------------------------------------------

infixr 5 :>

data Stream a = a :> Stream a
  deriving Eq  -- bogus, but necessary prior to GHC 7.4 for the Num instance later
           
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

------------------------------------------------------------
-- #4. Stream utilities
------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = s
  where s = x :> s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :> xs) = f x :> streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a :> streamFromSeed f (f a)

------------------------------------------------------------
-- #5 Defining streams
------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleave :: Stream a -> Stream a -> Stream a
interleave (x :> xs) ys = x :> interleave ys xs

ruler :: Stream Integer
ruler = interleave (streamRepeat 0) (streamMap (+1) ruler)

------------------------------------------------------------
-- #6 Generating functions
------------------------------------------------------------

x :: Stream Integer
x = 0 :> 1 :> streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = x :> streamRepeat 0
  negate        = streamMap negate
  (x :> xs) + (y :> ys) = (x+y) :> (xs+ys)
  (x :> xs) * yys@(y :> ys) = (x*y) :> (streamMap (x*) ys + (xs * yys))

instance Fractional (Stream Integer) where
  (x :> xs) / (y :> ys) = qs
    where qs = (x `div` y) :> streamMap (`div` y) (xs - qs*ys)

fibs3 :: Stream Integer
fibs3 = x/(1 - x - x^2)

------------------------------------------------------------
-- #4. Matrices.
------------------------------------------------------------

data Matrix = M Integer Integer Integer Integer
  deriving (Eq, Show)

instance Num Matrix where
--  fromInteger n = M n 0
--                    0 n
--  negate (M a11 a12 a21 a22) = M (negate a11) (negate a12) (negate a21) (negate a22)
--  (M a11 a12
--     a21 a22) + (M b11 b12
--                   b21 b22) = M (a11 + b11) (a12 + b12)
--                                (a21 + b21) (a22 + b22)
  (M a11 a12
     a21 a22) * (M b11 b12
                   b21 b22) = M (a11*b11 + a12*b21) (a11*b12 + a12*b22)
                                (a21*b11 + a22*b21) (a21*b12 + a22*b22)

fibM :: Matrix
fibM = M 1 1 1 0

proj12 :: Matrix -> Integer
proj12 (M _ x _ _) = x

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = proj12 (fibM^n) 

------------------------------------------------------------
-- #5. Memoization of naive recursive version.
------------------------------------------------------------

memoized_fib :: Int -> Integer
memoized_fib =
   let fib 0 = 0
       fib 1 = 1
       fib n = memoized_fib (n-2) + memoized_fib (n-1)
   in  (map fib [0 ..] !!)

fibs5 :: [Integer]
fibs5 = map memoized_fib [0..]