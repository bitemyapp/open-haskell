
{-# OPTIONS -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (liftM, replicateM)
import Control.Monad.Random
import Data.List (sort)

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

tag :: (Int, a) -> (DieValue, a)
tag (i, x) = (DV i, x)

instance Random DieValue where
  random g = tag $ randomR (1,6) g
  randomR (low,hi) g = tag $ randomR (max 1 (unDV low), min 6 (unDV hi)) g

die :: Rand StdGen DieValue
die = getRandom

type Army = Int

type Battlefield = (Army, Army)

numberOfDie :: Battlefield -> (Int, Int)
numberOfDie (attack, defend)
  | attack <= 1 = (0,defend)
  | defend <= 0 = (attack,0)
  | otherwise   = (min (attack - 1) 3, min defend 2)

clearDieRolls :: [(DieValue, DieValue)] -> Battlefield -> Battlefield
clearDieRolls [] b = b
clearDieRolls ((DV red, DV white):rolls) (attack, defend)
  | red > white = clearDieRolls rolls (attack, defend - 1)
  | otherwise   = clearDieRolls rolls (attack - 1, defend)

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
  let (numRed, numWhite) = numberOfDie battlefield
  redDie   <- liftM (reverse . sort) $ replicateM numRed getRandom
  whiteDie <- liftM (reverse . sort) $ replicateM numWhite getRandom
  return $ clearDieRolls (zip redDie whiteDie) battlefield

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(attack,defend)
  | attack > 1 && defend > 0 = do battlefield' <- battle battlefield
                                  invade battlefield'
  | otherwise                = return battlefield

type Campaign = (Army, [Army])

rampage :: Campaign -> Rand StdGen Campaign
rampage campaign@(_, []) = return campaign
rampage campaign@(attack, (defend : remaining))
  | attack <= 1 = return campaign
  | defend <= 0 = rampage (attack - 1, remaining)
  | otherwise   = do (attack', defend') <- invade (attack, defend)
                     rampage (attack', defend' : remaining)

test :: Show a => Rand StdGen a -> IO ()
test rand = evalRandIO rand >>= putStrLn . show

testPlay :: IO ()
testPlay = test $ rampage (20, [4,3,4])
