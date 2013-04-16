{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, StandaloneDeriving #-}

module HW10Test (tests, main) where

import Control.Applicative

import Data.Char

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import AParser
import SExpr

tests = [ testGroup "ex1/Functor"
          [ testCase "functor OK"   testFunctor1
          , testCase "functor fail" testFunctor2
          , testProperty "functor rand" prop_Functor
          ]

        , testGroup "ex2/Applicative"
          [ testCase "applicative OK" testApp1
          , testCase "applicative fail" testApp2
          , testProperty "applicative rand" prop_applicative
          ]

        , testGroup "ex3/Alternative"
          [ testCase "alternative1" testAlt1
          , testCase "alternative2" testAlt2
          , testCase "alternative3" testAlt3
          , testCase "alternative4" testAlt4
          , testCase "alternative5" testAlt5
          ]

        , testGroup "ex4/repetition"
          [ testCase "repZ1" testRepZ1
          , testCase "repZ2" testRepZ2
          , testCase "repZ3" testRepZ3
          , testCase "repO1" testRepO1
          , testCase "repO2" testRepO2
          , testCase "repO3" testRepO3
          , testCase "repE1" testRepE1
          , testCase "repE2" testRepE2
          , testCase "repE3" testRepE3
          , testCase "repE4" testRepE4                     
          ]

        , testGroup "ex5/util"
          [ testCase "spaces1" testSpaces1
          , testCase "spaces2" testSpaces2

          , testCase "testIdentE1" testIdentE1
          , testCase "testIdentE2" testIdentE2
          , testCase "testIdentE3" testIdentE3
          , testCase "testIdentE4" testIdentE4

          , testCase "testIdentSingle" testIdentSingle
          ]

        , testGroup "ex6/SExpr"
          [ testCase "sexpr1" testSE1
          , testCase "sexpr2" testSE2
          , testCase "sexpr3" testSE3
          , testCase "sexpr4" testSE4
          , testCase "sexpr5" testSE5
          ]
        ]

main = defaultMain tests

-- Exercise 1 / Functor

testFunctor1 = runParser (fmap (+2) posInt) "345" @?= Just (347, "")
testFunctor2 = runParser (fmap (+2) posInt) "bf4" @?= Nothing

prop_Functor :: Blind (Integer -> Integer) -> Positive Integer -> Bool
prop_Functor (Blind f) (Positive n) = runParser (fmap f posInt) (show n) == Just (f n, "")

-- Exercise 2 / Applicative

p2 = ((+) <$> posInt <*> (char ' ' *> posInt))

testApp1 = runParser p2 "345 678" @?= Just (1023, "")
testApp2 = runParser p2 "345 b6" @?= Nothing

prop_applicative :: Blind (Integer -> Integer -> Integer) -> Positive Integer -> Positive Integer -> Bool
prop_applicative (Blind f) (Positive m) (Positive n) = 
  runParser (f <$> posInt <*> (char ' ' *> posInt)) (show m ++ " " ++ show n) == Just (f m n, "")

-- Exercise 3 / Alternative

p3 = posInt <|> pure 6
p4 = char 'x' <|> char 'y'

testAlt1 = runParser p3 "234" @?= Just (234, "")
testAlt2 = runParser p3 "x" @?= Just (6, "x")
testAlt3 = runParser p4 "x" @?= Just ('x', "")
testAlt4 = runParser p4 "y" @?= Just ('y', "")
testAlt5 = runParser p4 "z" @?= Nothing

-- Exercise 4 / zeroOrMore/oneOrMore

testRepZ1 = runParser (zeroOrMore p4) "xyxxy" @?= Just ("xyxxy", "")
testRepZ2 = runParser (zeroOrMore p4) "xyxxyzyyx" @?= Just ("xyxxy", "zyyx")
testRepZ3 = runParser (zeroOrMore p4) "" @?= Just ("", "")
testRepO1 = runParser (oneOrMore p4) "xyxxy" @?= Just ("xyxxy", "")
testRepO2 = runParser (oneOrMore p4) "xyxxyzyyx" @?= Just ("xyxxy", "zyyx")
testRepO3 = runParser (oneOrMore p4) "" @?= Nothing

testRepE1 = runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" 
            @?= Just ("ABC","dEfgH")
testRepE2 = runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
            @?= Just ("ABC","dEfgH")
testRepE3 = runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
            @?= Just ("","abcdeFGh")
testRepE4 = runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
            @?= Nothing

-- Exercise 5 / util

testSpaces1 = runParser spaces "   " @?= Just ("   ", "")
testSpaces2 = runParser spaces sp @?= Just (sp, "")
  where sp = "\n\t \t\r" 

testIdentE1 = runParser ident "foobar baz"
              @?= Just ("foobar"," baz")
testIdentE2 = runParser ident "foo33fA"
              @?= Just ("foo33fA","")
testIdentE3 = runParser ident "2bad"
              @?= Nothing
testIdentE4 = runParser ident ""
              @?= Nothing

testIdentSingle = runParser ident "x"
                  @?= Just ("x", "")

-- Exercise 6 / S-exprs

deriving instance Eq Atom
deriving instance Eq SExpr

n = A . N
i = A . I

testSE1 = runParser parseSExpr "5" @?= Just (A (N 5), "")
testSE2 = runParser parseSExpr "foo3" @?= Just (A (I "foo3"), "")
testSE3 = runParser parseSExpr "(bar (foo) 3 5 874)"
          @?= Just (Comb [i "bar", Comb [i "foo"], n 3, n 5, n 874], "")
testSE4 = runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
          @?= Just (Comb 
                    [ Comb
                      [ Comb
                        [ i "lambda"
                        , i "x"
                        , Comb
                          [ i "lambda"
                          , i "y"
                          , Comb
                            [ i "plus"
                            , i "x"
                            , i "y"
                            ]
                          ]
                        ]
                      , n 3
                      ]
                    , n 5
                    ]
                   , ""
                   )
testSE5 = runParser parseSExpr "(  lots of   ( spaces  in )  this ( one ) )"
          @?= Just (Comb
                    [ i "lots", i "of"
                    , Comb [ i "spaces", i "in" ]
                    , i "this"
                    , Comb [ i "one" ]
                    ], "")
