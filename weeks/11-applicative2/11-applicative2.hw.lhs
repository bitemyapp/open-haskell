% -*- LaTeX -*-
\documentclass{article}
%include lhs2TeX.fmt
\usepackage{graphicx}
\usepackage[stable]{footmisc}
\usepackage{../hshw}

\usepackage{amsmath}

\begin{document}

\title{CIS 194: Homework 10}
\date{}
\author{Due Thursday, March 29}
\maketitle

\begin{itemize}
\item Files you should submit: |AParser.hs| and |SExpr.hs|.  You should
  take the versions that we have provided and add your solutions to
  them.
\end{itemize}

\section{Introduction}

\begin{center}
  \includegraphics[width=2in]{parsley.jpg}
\end{center}

A \emph{parser} is an algorithm which takes unstructured data as input
(often a |String|) and produces structured data as output.  For
example, when you load a Haskell file into |ghci|, the first thing it
does is \emph{parse} your file in order to turn it from a long
|String| into an \emph{abstract syntax tree} representing your code in
a more structured form.

Concretely, we will represent a parser for a value of type |a| as a
function which takes a |String| represnting the input to be parsed,
and succeeds or fails; if it succeeds, it returns the parsed value
along with whatever part of the input it did not use.
\begin{spec}
newtype Parser a 
  = Parser { runParser :: String -> Maybe (a, String) }  
\end{spec}

For example, |satisfy| takes a |Char| predicate and constructs a
parser which succeeds only if it sees a |Char| that satisfies the
predicate (which it then returns).  If it encounters a |Char| that
does not satisfy the predicate (or an empty input), it fails.
\newpage
\begin{spec}
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail
\end{spec}

Using satisfy, we can also define the parser |char|, which expects to
see exactly a given character and fails otherwise.
\begin{spec}
char :: Char -> Parser Char
char c = satisfy (== c)
\end{spec}

For example:

\begin{verbatim}
*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")
\end{verbatim}

For convenience, we've also provided you with a parser for positive
integers:

\begin{spec}
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs
\end{spec}

\section{Tools for building parsers}
\label{sec:tools}

However, implementing parsers explicitly like this is tedious and
error-prone for anything beyond the most basic primitive parsers.  The
real power of this approach comes from the ability to create complex
parsers by \emph{combining} simpler ones.  And this power of combining
will be given to us by\dots you guessed it, |Applicative|.

\exercise

First, you'll need to implement a |Functor| instance for
|Parser|. \emph{Hint}: You may find it useful to implement a function

\begin{spec}
first :: (a -> b) -> (a,c) -> (b,c)
\end{spec}

\exercise

Now implement an |Applicative| instance for |Parser|:
\begin{itemize}
\item |pure a| represents the parser which consumes no input and
  successfully returns a result of |a|.
\item |p1 <*> p2| represents the parser which first runs |p1| (which
  will consume some input and produce a function), then passes the
  \emph{remaining} input to |p2| (which consumes more input and
  produces some value), then returns the result of applying the
  function to the value.  However, if either |p1| or |p2| fails then
  the whole thing should also fail (put another way, |p1 <*> p2| only
  succeeds if both |p1| and |p2| succeed).
\end{itemize}

So what is this good for?  Recalling the |Employee| example from
class,
\begin{spec}
type Name = String
data Employee = Emp { name :: Name, phone :: String }
\end{spec}
we could now use the |Applicative| instance for |Parser| to
make an employee parser from name and phone parsers.  That is, if
\begin{spec}
parseName  :: Parser Name
parsePhone :: Parser String
\end{spec}
then 
\begin{spec}
Emp <$> parseName <*> parsePhone :: Parser Employee
\end{spec}
% $
is a parser which first reads a name from the input, then a phone
number, and returns them combined into an |Employee| record.  Of
course, this assumes that the name and phone number are right next to
each other in the input, with no intervening separators.  We'll see
later how to make parsers that can throw away extra stuff that doesn't
directly correspond to information you want to parse.

You can also test your |Applicative| instance using other simple
applications of functions to multiple parsers:

\begin{verbatim}
*Parser> runParser ((,) <$> char 'a' <*> char 'b') "abc"
Just (('a','b'),"c")
*Parser> runParser ((\x _ y -> x + y) 
                      <$> posInt <*> char ' ' <*> posInt) "12 34"
Just (46,"")
\end{verbatim}

Note: it's worth spending some time to make sure you understand how
the above examples work before moving on!

\exercise

|Applicative| by itself can be used to make parsers for simple, fixed
formats.  But for any format involving \emph{choice} (\emph{e.g.}
``\dots after the colon there can be a number \textbf{or} a word
\textbf{or} parentheses\dots'') |Applicative| is not quite enough.  To
handle choice we turn to the |Alternative| class, defined
(essentially) as follows:

\begin{spec}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
\end{spec}

|(<||>)| is intended to represent \emph{choice}: that is, |f1 <||> f2|
represents a choice between |f1| and |f2|.  |empty| should be the
identity element for |(<||>)|, and often represents \emph{failure}.

Write an |Alternative| instance for |Parser|:
\begin{itemize}
\item |empty| represents the parser which always fails.
\item |p1 <||> p2| represents the parser which first tries running
  |p1|. If |p1| succeeds then |p2| is ignored and the result of |p1|
  is returned. Otherwise, if |p1| fails, then |p2| is tried instead.
\end{itemize}
\emph{Hint}: there is already an |Alternative| instance for |Maybe|
which you may find useful.

\section{Parsing S-expressions}
\label{sec:sexpr}

At this point, what do we have?
\begin{itemize}
\item the definition of a basic |Parser| type
\item a few primitive parsers such as |satisfy|, |char|, and |posInt|
\item |Functor|, |Applicative|, and |Alternative| instances for |Parser|
\end{itemize}
So, what can we do with this?  It may not seem like we have much to go
on, but it turns out we can actually do quite a lot.

From this point onward, you should be able to code everything directly
in terms of the primitive parsers we started with, and the |Functor|,
|Applicative| and |Alternative| interfaces you made for parsers.  You
should \emph{not} have to spend any more time calling |runParser|,
dealing with |Maybe|, and so on!  All those messy details are handled
for you by the |Applicative| and |Alternative| instances.  You can now
just concentrate on combining parsers at a higher level without
worrying about the internal details of how they are implemented.

\exercise

First, let's see how to take a parser for (say) widgets and turn it
into a parser for \emph{lists} of widgets.  In particular, there are
two functions you should implement: |zeroOrMore| takes a parser as
input and runs it consecutively as many times as possible (which could
be none, if it fails right away), returning a list of the
results. |zeroOrMore| always succeeds.  |oneOrMore| is similar, except
that it requires the input parser to succeed at least once.  If the
input parser fails right away then |oneOrMore| also fails.

For example, below we use |zeroOrMore| and |oneOrMore| to parse a
sequence of uppercase characters.  The longest possible sequence of
uppercase characters is returned as a list.  In this case,
|zeroOrMore| and |oneOrMore| behave identically:
\begin{verbatim}
*Parser> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")
*Parser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")
\end{verbatim}

The difference between them can be seen when there is not an uppercase
character at the beginning of the input.  |zeroOrMore| succeeds and
returns the empty list without consuming any input; |oneOrMore| fails.
\begin{verbatim}
*Parser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
Just ("","abcdeFGh")
*Parser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
Nothing
\end{verbatim}

Implement |zeroOrMore| and |oneOrMore| with the following type
signatures:

\begin{spec}
zeroOrMore :: Parser a -> Parser [a]
oneOrMore  :: Parser a -> Parser [a]
\end{spec}

(\emph{Hint}: To parse one or more occurrences of |p|, run |p| once and
then parse zero or more occurrences of |p|.  To parse zero or more
occurrences of |p|, try parsing one or more; if that fails, return the
empty list.  (That might sound circular, but it isn't!))

\exercise

There are a few more utility parsers needed before we can accomplish
the final parsing task. First, |spaces| should parse a consecutive
list of zero or more whitespace characters (use the |isSpace| function
from |Data.Char|).

\begin{spec}
spaces :: Parser String
\end{spec}

Next, |ident| should parse an \emph{identifier}, which for our
purposes will be an alphabetic character (use |isAlpha|) followed by
zero or more alphanumeric characters (use |isAlphaNum|).  In other
words, an identifier can be any nonempty sequence of letters and
digits except that it may not start with a digit.

\begin{spec}
ident :: Parser String
\end{spec}

For example:

\begin{verbatim}
*Parser> runParser ident "foobar baz"
Just ("foobar"," baz")
*Parser> runParser ident "foo33fA"
Just ("foo33fA","")
*Parser> runParser ident "2bad"
Nothing
*Parser> runParser ident ""
Nothing
\end{verbatim}

\exercise

\emph{S-expressions} are a simple syntactic format for tree-structured
data, originally developed as a syntax for Lisp programs.  We'll close
out our demonstration of parser combinators by writing a simple
S-expression parser.

An \emph{identifier} is represented as just a |String|; the format for
valid identifiers is represented by the |ident| parser you wrote in
the previous exercise.
\begin{spec}
type Ident = String
\end{spec}

An ``atom'' is either an integer value (which can be parsed with
|posInt|) or an identifier.
\begin{spec}
data Atom = N Integer | I Ident
  deriving Show
\end{spec}

Finally, an S-expression is either an atom, or a list of
S-expressions.\footnote{Actually, this is slightly different than the
  usual definition of S-expressions in Lisp, which also includes binary
  ``cons'' cells; but it's good enough for our purposes.}
\begin{spec}
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
\end{spec}

Textually, S-expressions can optionally begin and end with any number
of spaces; after \emph{throwing away leading and trailing spaces} they
consist of either an atom, \emph{or} an open parenthesis followed by
one or more S-expressions followed by a close parenthesis.

\begin{align*}
  atom &::= int \\
       &\mid ident \\
\\
  S &::= atom \\
    &\mid \text{|(|} S^* \text{|)|}
\end{align*}

For example, the following are all valid S-expressions:

\begin{verbatim}
5
foo3
(bar (foo) 3 5 874)
(((lambda x (lambda y (plus x y))) 3) 5)
(   lots  of   (  spaces   in  )  this ( one ) )
\end{verbatim}

We have provided Haskell data types representing S-expressions in
|SExpr.hs|.  Write a parser for S-expressions, that is, something of
type

\begin{spec}
parseSExpr :: Parser SExpr
\end{spec}

\emph{Hints}: To parse something but ignore its output, you can use
the |(*>)| and |(<*)| operators, which have the types

\begin{spec}
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a
\end{spec}

|p1 *> p2| runs |p1| and |p2| in sequence, but ignores the result of
|p1| and just returns the result of |p2|.  |p1 <* p2| also runs |p1|
and |p2| in sequence, but returns the result of |p1| (ignoring |p2|'s
result) instead.

For example:
\begin{verbatim}
*Parser> runParser (spaces *> posInt) "     345"
Just (345,"")
\end{verbatim}

\end{document}

% Local Variables:
% mode:latex
% compile-command:"make hw"
% End:
