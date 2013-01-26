{-# OPTIONS -Wall #-}

{-
 - Authors: John Mayer <jpmayer@seas.upenn.edu>,
 -          Adi Dahiya <adahiya@seas.upenn.edu>
 - Date:    January 26, 2013
 -}
module GolfSol where

import Data.List

------------------------------------------------------------------------------
-- Exercise 1

-- | Generate a list of n 'skips' for the given list l, where n is the length of
--   l. The ith 'skip' is the result of filtering every ith element from l.
skips :: [a] -> [[a]]
skips l = map (f l) [1 .. length l] where
  f :: [a] -> Int -> [a]
  f l' n = case drop (n - 1) l' of
             (x:xs) -> x : f xs n
             []     -> []

------------------------------------------------------------------------------
-- Exercise 2

-- | Find all the local maxima in a list. A local maxima is defined as an
--   element that is larger than *both* its neighbors.
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) | a < b && b > c = b : r
                       | True           = r
  where r = localMaxima (b:c:xs)
localMaxima _ = []

------------------------------------------------------------------------------
-- Exercise 3

histogram :: [Integer] -> String
histogram [] = ""
histogram l = unlines $ (transpose bars) ++ ["==========", "0123456789"] where

  -- Create a horizontal bar graph by repeating ' ' maxOcurrences - i times and
  -- then repeating '*' i times (for each instance in the original list).
  bars :: [String]
  bars = map (\i -> take (maximum counts - i) (repeat ' ') ++
                    take i (repeat '*')) counts

  -- Count the occurrences of each integer 0..9 in the input data.
  counts :: [Int]
  counts = map (\i -> length $ elemIndices i l) [0 .. 9]


-- Alternate solution
histogram' :: [Integer] -> String
histogram' l = unlines . reverse $ "0123456789" : "==========" : chart where

  chart :: [String]
  chart = takeWhile (elem '*') [chartLine l n | n <- [1..]]

  chartLine :: [Integer] -> Int -> String
  chartLine l' n = plot [d | d <- [0..9], atleast n d l']

  atleast :: Int -> Integer -> [Integer] -> Bool
  atleast n _ []     = n == 0
  atleast n i (x:xs)
    | n == 0    = True
    | i == x    = atleast (n-1) i xs
    | otherwise = atleast n     i xs

  plot :: [Integer] -> String
  plot is = [if elem i is then '*' else ' ' | i <- [0..9]]

testHist :: [Integer] -> IO ()
testHist = putStr . histogram
