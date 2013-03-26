
{-# OPTIONS -Wall #-}

module Party where

import           Data.List   (sort)
import           Data.Monoid
import           Data.Tree

import           Employee

-- Exercise 1: Guest lists

glDataCons :: [Employee] -> Fun -> GuestList
glDataCons = GL

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL es totFun) = GL (e:es) (fun + totFun)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 f1) `mappend` (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 >= f2  = gl1
  | otherwise = gl2

-- Exercise 2: Tree fold

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a cs) = f a (map (treeFold f) cs)

-- Exercise 3: One step in the algorithm

considerBoss :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
considerBoss e gls = (glCons e bestWith, bestWithout)
  where bestWith    = mconcat (snd (unzip gls))
        bestWithout = mconcat (map (uncurry moreFun) gls)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = considerBoss

-- Exercise 4: putting it together.

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold considerBoss


-- Exercise 5: reading in a larger tree.

main :: IO ()
main = readFile "company.txt" >>= \s -> putStr (solveGuestList s)

solveGuestList :: String -> String
solveGuestList = formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL emps fun) =
  "Total fun: " ++ show fun ++ "\n" ++ unlines (sort . map empName $ emps)
