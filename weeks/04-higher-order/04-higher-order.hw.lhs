% -*- LaTeX -*-
\documentclass{article}
%include lhs2TeX.fmt

\usepackage{../hshw}

\begin{document}

\title{CIS 194: Homework 4}
\date{}
\author{Due Thursday, February 9}
\maketitle


\textbf{What to turn in}: you should turn a single |.hs| (or |.lhs|)
file, which \textbf{must type check}.

\exerciset{Higher-order programming}

Implement a function with each of the following type signatures.
(There may be more than one correct solution for a given type
signature---any function which can be assigned the given type
signature is a correct solution.)  However, your solutions should
still use good style!

\begin{enumerate}
\item |(a -> a -> b) -> [a] -> Maybe b|
\item |(a -> c) -> (b -> c) -> [Either a b] -> [c]|
\item |((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)|
\end{enumerate}

As a clarification, it is OK if your solutions are \emph{more general}
than the required type.  As an example, if you were required to write
a function of type |Int -> Int|, it would be acceptable to write
\begin{code}
f :: Int -> Int
f x = x
\end{code}
since this typechecks, even though \texttt{ghc} will infer the more
general type |a -> a| for |f| if the type signature is removed.

\exerciset{Wholemeal programming}

Reimplement each of the following functions in a more idiomatic
Haskell style.  Use \emph{wholemeal programming} practices, breaking
each function into a sequence of incremental transformations of an
entire data structure.

\begin{enumerate}
\item 
\begin{code}
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs
\end{code}

\item
\begin{code}
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
\end{code}

Hint: For this problem you may wish to use the functions |iterate| and
|takeWhile|.  Look them up in the Prelude documentation to see what
they do.
\end{enumerate}

\exerciset{Folds}

\begin{enumerate}
\item Implement a function
\begin{code}
xor :: [Bool] -> Bool    
\end{code}
which returns |True| if and only if there are an odd number of |True|
values contained in the input list.  It does not matter how many
|False| values the input list contains.  For example,

|xor [False, True, False] == True|

|xor [False, True, False, False, True] == False|

Your solution must be implemented using a fold.

\item Implement |map| as a fold.  That is, complete the definition

\begin{code}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ...
\end{code}

  in such a way that |map'| behaves identically to the standard |map|
  function.
\end{enumerate}

\end{document}