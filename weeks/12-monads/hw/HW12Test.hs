import System.Random
import Control.Monad.Random
import Control.Monad (liftM, liftM2, replicateM)
import Data.List (sort)


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

import Risk (Battlefield(Battlefield), Army, DieValue(DV))
import qualified Risk as T


tests = [ testGroup "ex2/battle"
          [ testProperty "battle"   prop_battle
          ]
        ]

main = defaultMain tests

-- | Evaluate a random computation in the IO monad,
--   creating a new random generator with a seed
ioWithSeed :: Int -> Rand StdGen a -> IO a
ioWithSeed s x = fmap (evalRand x) (return $ mkStdGen s)


instance Arbitrary Battlefield where
--  arbitrary = (liftM2 Battlefield) arbitrary arbitrary
  arbitrary = do
    a <- arbitrary
    d <- arbitrary
    return $ Battlefield (a `mod` 10) (d `mod` 10)

instance Eq Battlefield where
  (Battlefield a d) == (Battlefield a' d') = (a, d) == (a', d')

instance Show Battlefield where
  show (Battlefield a d) = "(" ++ show a ++ ", " ++ show d ++ ")"

-- Exercise 2
prop_battle :: Int -> Battlefield -> Property
prop_battle s b = monadicIO t where
  t = do
    b' <- run $ ioWithSeed s $ battle b
    b'' <- run $ ioWithSeed s $ T.battle b
    assert $ b' == b''


-- Solution functions
numberOfDie :: Battlefield -> (Int, Int)
numberOfDie (Battlefield attack defend)
  | attack <= 1 = (0,defend)
  | defend <= 0 = (attack,0)
  | otherwise   = (min (attack - 1) 3, min defend 2)

clearDieRolls :: [(DieValue, DieValue)] -> Battlefield -> Battlefield
clearDieRolls [] b = b
clearDieRolls ((DV red, DV white):rolls) (Battlefield attack defend)
  | red > white = clearDieRolls rolls (Battlefield attack (defend - 1))
  | otherwise   = clearDieRolls rolls (Battlefield (attack - 1) defend)

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
  let (numRed, numWhite) = numberOfDie battlefield
  redDie   <- liftM (reverse . sort) $ replicateM numRed getRandom
  whiteDie <- liftM (reverse . sort) $ replicateM numWhite getRandom
  return $ clearDieRolls (zip redDie whiteDie) battlefield

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(Battlefield attack defend)
  | attack > 1 && defend > 0 = do battlefield' <- battle battlefield
                                  invade battlefield'
  | otherwise                = return battlefield

type Campaign = (Army, [Army])

rampage :: Campaign -> Rand StdGen Campaign
rampage campaign@(_, []) = return campaign
rampage campaign@(attack, (defend : remaining))
  | attack <= 1 = return campaign
  | defend <= 0 = rampage (attack - 1, remaining)
  | otherwise   = do
    (Battlefield attack' defend') <- invade (Battlefield attack defend)
    rampage (attack', defend' : remaining)
