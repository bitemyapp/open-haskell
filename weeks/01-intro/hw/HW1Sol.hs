-- CIS 194, Fall 2010
--
-- Sample solution for HW 1.  For each problem, a solution is provided
-- which models a good solution along the lines we were expecting. For
-- some exercises, an alternate version is also shown which is more
-- idiomatic Haskell (though we would not have expected you to come up
-- with it at this point in the course).

module HW1Sol where

-- Exercise 1 -----------------------------------------

-- Find the digits of a number in reverse order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n < 10    = [n]
              | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Find the digits of a number in their original order.
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Alternate version:
toDigits' = reverse . toDigitsRev

-- Exercise 2 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []      = []
doubleEveryOther [x]     = [x]
doubleEveryOther (x:y:z) = x : 2*y : doubleEveryOther z

-- Alternate version using zipWith.
doubleEveryOther' = zipWith (*) (cycle [1,2])

-- Exercise 3 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = mySum (toDigits x) + sumDigits xs

mySum :: [Integer] -> Integer
mySum []     = 0
mySum (x:xs) = x + mySum xs

-- Alternate approach that seperates the flattening of the digit list.
sumDigits2 n = mySum (explode n)

explode :: [Integer] -> [Integer]
explode []     = []
explode (x:xs) = toDigits x ++ explode xs

-- A more idiomatic version:
sumDigits' = sum . concatMap toDigits

-- Exercise 4 -----------------------------------------

-- Validate a credit card number using the above functions.
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

-- Alternate version, now that we know about function composition and
-- operator sections:
validate' = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

-- Exercise 5 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a
