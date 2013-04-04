{- CIS 194 HW 10
   due Thursday, 29 March
-}

module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail


-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

singleInt :: Parser Integer
singleInt = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | isDigit x = Just (read [x], xs)
      | otherwise = Nothing
    -- f xs
    --   | null ns   = Nothing
    --   | otherwise = Just (read (head xs), tail xs)
    --   --   Just (read ns, rest)
    --   -- where (ns:rest) = x:xs


eof :: Parser ()
eof = Parser f
  where f [] = Just ((), [])
        f _  = Nothing

type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName :: Parser Name
parseName = undefined
parsePhone :: Parser String
parsePhone = undefined
------------------------------------------------------------
--  Exercise #1: Functor instance for Parser
------------------------------------------------------------

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where

  -- How we might expect you to write it:
  fmap f (Parser p) = Parser (\s -> fmap (first f) (p s))

  -- or perhaps:

  -- fmap f (Parser p) = Parser (fmap (first f) . p)

  -- An alternative definition using a Semantic Editor Combinator
  -- style, see
  -- http://conal.net/blog/posts/semantic-editor-combinators:

  -- fmap = inParser . fmap . fmap . first

------------------------------------------------------------
--  Exercise #2: Applicative instance for Parser
------------------------------------------------------------

instance Applicative Parser where
  -- a -> Parser a
  pure a = Parser (\s -> Just (a, s))
  -- Parser (p1 -> p2) -> Parser p1 -> Parser p2
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (fmap f xp) s'

------------------------------------------------------------
--  Exercise #3: Implementing Parsers
------------------------------------------------------------
abParser :: Parser (Char, Char)
abParser = (\a b -> (a, b)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

------------------------------------------------------------
--  Exercise #4: Alternative instance for Parser
------------------------------------------------------------

instance Alternative Parser where
  empty     = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

------------------------------------------------------------
--  Exercise #5: intOrUppercase
------------------------------------------------------------

intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
