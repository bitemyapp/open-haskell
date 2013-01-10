
{-# OPTIONS -Wall #-}

module TestGolf where

-- import testing frameworks

-- import students' solution file
import Golf (skips, localMaxima, histogram)

-- working solution (jpmayer)
skips' :: [a] -> [[a]]
skips' l = [skipN n l | n <- [1..length l]] where
  skipN n l' = skipKN 1 n l'
  skipKN _ _ [] = []
  skipKN k n (x:xs)
    | k < n     = skipKN (k+1) n xs
    | otherwise = x : skipKN 1 n xs

localMaxima' :: [Integer] -> [Integer]
localMaxima' (a:b:c:xs)
  | b > a && b > c = b : r
  | True = r where 
    r = localMaxima' (b:c:xs)
localMaxima' _ = []

histogram' :: [Integer] -> String
histogram' l = unlines.reverse
                     $ "0123456789"
                     : "=========="
                     : chart l where

chart :: [Integer] -> [String]
chart l = takeWhile (elem '*') [chartLine l n | n <- [1..]]
  
chartLine :: [Integer] -> Int -> String
chartLine l n = plot [d | d <- [0..9], atleast n d l]
  
atleast :: Int -> Integer -> [Integer] -> Bool
atleast n _ []     = n == 0
atleast n i (x:xs) 
  | n == 0    = True
  | i == x    = atleast (n-1) i xs
  | otherwise = atleast n     i xs
    
plot :: [Integer] -> String
plot is = [if elem i is then '*' else ' ' | i <- [0..9]]
  
testHist :: [Integer] -> IO ()
testHist = putStr . histogram'

-- Begin test code

