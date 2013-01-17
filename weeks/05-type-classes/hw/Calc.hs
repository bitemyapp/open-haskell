{-# LANGUAGE FlexibleInstances,
             TypeSynonymInstances,
             GeneralizedNewtypeDeriving #-}

-- CIS 194, Spring 2012
-- HW 5 sample solution

module Calc where

import ExprT
import Parser
import qualified StackVM as S

import qualified Data.Map as M
import Control.Applicative

----------------
-- Exercise 1 --
----------------

eval :: ExprT -> Integer
eval (Lit i)   = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

----------------
-- Exercise 2 --
----------------

-- Approach 1: create a function to optionally apply eval to a Maybe.
evalStr :: String -> Maybe Integer
evalStr = evalMay . parseExp Lit Add Mul
  where evalMay Nothing  = Nothing
        evalMay (Just e) = Just $ eval e

-- But as we now know, we can apply eval to a Maybe with fmap.
evalStr' :: String -> Maybe Integer
evalStr' = fmap eval . parseExp Lit Add Mul

----------------
-- Exercise 3 --
----------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

----------------
-- Exercise 4 --
----------------

-- Look ma, no parameters!

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show,Eq,Ord,Num)
instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Saturated = Saturated Integer deriving (Show,Eq,Num)
instance Expr Saturated where
  lit = Saturated . max 0 . min 7
  add (Saturated x) (Saturated y) = lit (x + y)
  mul (Saturated x) (Saturated y) = lit (x * y)

----------------
-- Exercise 5 --
----------------

instance Expr S.Program where
    lit x = [S.PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

----------------
-- Exercise 6 --
----------------

class HasVars a where
  var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

liftMaybe2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
liftMaybe2 _ Nothing _ = Nothing
liftMaybe2 _ _ Nothing = Nothing
liftMaybe2 f (Just a) (Just b) = Just (f a b)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit         = const . Just
  add e1 e2 m = liftMaybe2 (+) (e1 m) (e2 m)
  mul e1 e2 m = liftMaybe2 (*) (e1 m) (e2 m)

-- Or, as we will learn shortly,
{-
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit         = const . Just
  add e1 e2 m = (+) <$> e1 m <*> e2 m
  mul e1 e2 m = (*) <$> e1 m <*> e2 m
-}

-- Or even! (This is overdoing it a bit...)
{-
(<$.>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$.>) = fmap . fmap

(<*.>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<*.>) = (<*>) . fmap (<*>)

liftAA2 f x y = f <$.> x <*.> y

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftAA2 (+)
  mul = liftAA2 (*)
-}

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
