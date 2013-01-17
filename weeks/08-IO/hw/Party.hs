
{-# OPTIONS -Wall #-}

module Party where

import Data.List (sort)
import Data.Tree
import Data.Monoid

import Employee

-- Exercise 1: Guest lists

data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

glGetFun :: GuestList -> Fun
glGetFun (GL _ f) = f

glDataCons :: [Employee] -> Fun -> GuestList
glDataCons = GL

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL es totFun) = GL (e:es) (fun + totFun)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 f1) `mappend` (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

-- Exercise 2: Tree fold
  
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a cs) = f a (map (treeFold f) cs)

-- Exercise 3: One step in the algorithm

considerBoss :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
considerBoss e gls = (glCons e bestWith, bestWithout)
  where bestWith    = mconcat (snd (unzip gls))
        bestWithout = mconcat (map (uncurry max) gls)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = considerBoss

-- Exercise 4: putting it together.

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold considerBoss

-- Exercise 5: reading in a larger tree.

main :: IO ()
main = readFile "company.txt" >>= \s -> putStr (solveGuestList s)

solveGuestList :: String -> String
solveGuestList = formatGL . maxFun . read 

formatGL :: GuestList -> String
formatGL (GL emps fun) = 
  "Total fun: " ++ show fun ++ "\n" ++ unlines (sort . map empName $ emps)
