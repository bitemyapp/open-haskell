{- CIS 194 HW 10
   due Thursday, 29 March
-}

module SExpr where

import Control.Applicative

import AParser

------------------------------------------------------------
--  Exercise #6: Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseAtom :: Parser Atom
parseAtom   = N <$> posInt
          <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *>
             (
                   A <$> parseAtom
               <|> Comb <$> parens (oneOrMore parseSExpr)
             ) <* spaces

parseLine :: Parser SExpr
parseLine = parseSExpr <* eof