{-# OPTIONS -Wall #-}

module HW4Sol where

------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming

-- Reimplement each of the following functions in a more idiomatic Haskell
-- style. Use wholemeal programming practices, breaking each function into
-- a sequence of incremental transformations of an entire data structure.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x->x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

hailStep :: Integer -> Integer
hailStep n
  | even n    = n `div` 2
  | otherwise = (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate hailStep

------------------------------------------------------------------------------
-- Exercise 2: Folding with trees

-- Recall the definition of a binary tree data structure
-- (http://en.wikipedia.org/wiki/Binary_tree). A branch of a binary tree is
-- balanced if the height of its left and right children differ by no more than
-- 1 (height at a node is the number of nodes you must traverse to reach the
-- deepest leaf). Write a function that generates a balanced binary tree from a
-- list of values using `foldr`.

-- Assume the data exists in the branches (nodes), as described in the following
-- data structure. Note that we keep track of the height of a subtree at each
-- node as an Integer:

data Tree a = Leaf
            | Branch Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf where

  treeInsert :: a -> Tree a -> Tree a
  treeInsert x Leaf = Branch 0 Leaf x Leaf
  treeInsert x (Branch h Leaf y Leaf) = Branch (h+1) (treeInsert x Leaf) y Leaf
  treeInsert x (Branch h l y r) =
    case l of
      Leaf            -> Branch h (treeInsert x l) y r
      Branch lh _ _ _ -> if h == lh + 1 -- If left subtree is taller / equal
                         then computeHeight l y (treeInsert x r)
                         else computeHeight (treeInsert x l) y r

  -- Generate a branch, making sure to compute the correct height of the subtree
  computeHeight :: Tree a -> a -> Tree a -> Tree a
  computeHeight l@(Branch lh _ _ _) x r@(Branch rh _ _ _) =
    Branch h' l x r where
      h' | lh > rh    = lh + 1
         | otherwise  = rh + 1
  computeHeight _ _ _ = error "shouldn't happen"

------------------------------------------------------------------------------
-- Exercise 3: More folds!

-- Apply the XOR logical function to a Bool list
xor :: [Bool] -> Bool
xor = foldl (/=) False

-- Write map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

-- Write foldl using foldr
-- Hint: Study how the application of `foldr` and `foldl` works out:
--  foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc y -> acc (f y x)) id xs base

------------------------------------------------------------------------------
-- Exercise 4: Finding primes

-- Read about the Sieve of Sundaram
-- (http://en.wikipedia.org/wiki/Sieve_of_Sundaram). Implement the algorithm
-- using function composition. Given an integer n, your function should generate
-- all the prime numbers up to 2n + 2. To give you some help, we've provided
-- a function to get the cartesian product of two lists.

-- This is similar to `zip`, but it produces all possible pairs instead of
-- matching up the list elements. It's written using list comprehensions, which
-- we haven't gotten to yet (but feel free to research them).
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) filtered where

  filtered :: [Integer]
  filtered = filter (`notElem` toFilter) [1..n]

  toFilter :: [Integer]
  toFilter = concatMap f [1..n]

  f :: Integer -> [Integer]
  f x = filter (<= n) (map (\(i, j) -> i+j+2*i*j) (cartProd [1..x] [1..n]))

-- A more advanced solution, just to give you a taste of the power of list
-- comprehensions and guards:
sieveSundaram' :: Int -> [Int]
sieveSundaram' n = [ x*2+1 | x <- [1..n],
                             x `notElem` [ i+j+2*i*j | j <- [1..n],
                                                       i <- [1..j],
                                                       i+j+2*i*j <= n ] ]
