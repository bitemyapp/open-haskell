

{-# OPTIONS -Wall #-}

module HW4Sol where

f1 :: (a -> a -> b) -> [a] -> Maybe b
f1 f (x:y:_) = Just $ f x y
f1 _ _ = Nothing

f2 :: (a -> c) -> (b -> c) -> [Either a b] -> [c]
f2 fl fr = map decide where

  decide (Left x) = fl x
  decide (Right y) = fr y

f3 :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
f3 arr abrr br = arr ((flip abrr) br)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x->x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

hailStep :: Integer -> Integer
hailStep n
  | even n    = n `div` 2
  | otherwise = (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate hailStep

-- Exercise 3 : 

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

