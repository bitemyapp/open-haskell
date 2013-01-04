% -*- LaTeX -*-
\documentclass{article}
%include lhs2TeX.fmt

\usepackage{../hshw}

\begin{document}

\title{CIS 194: Homework 1}
\date{}
\author{Due Monday, January 14}
\maketitle

When solving the homework, strive to create not just code that works,
but code that is stylish and concise.  See the style guide on the
website for some general guidelines.  Try to write small functions
which perform just a single task, and then combine those smaller
pieces to create more complex functions.  Don't repeat yourself: write
one function for each logical task, and reuse functions as necessary.

Be sure to write functions with exactly the specified name and type
signature for each exercise (to help us test your code).  You may
create additional helper functions with whatever names and type
signatures you wish.

\section{Validating Credit Card Numbers\footnote{Adapted from the
    first practicum assigned in the University of Utrecht functional
    programming course taught by Doaitse Swierstra, 2008-2009.}}

Have you ever wondered how websites validate your credit card number
when you shop online? They don't check a massive database of numbers,
and they don't use magic. In fact, most credit providers rely on a
checksum formula for distinguishing valid numbers from random
collections of digits (or typing mistakes).

\begin{center}
\includegraphics[width=3in]{images/credit-card.png}  
\end{center}


In this section, you will implement the validation algorithm for
credit cards. It follows these steps:

\begin{itemize}
  \item Double the value of every second digit beginning from the
    right.  That is, the last digit is unchanged; the
    second-to-last digit is doubled; the third-to-last digit
    is unchanged; and so on.
  \item Add the digits of the doubled values and the undoubled digits
    from the original number.
  \item Calculate the remainder when the sum is divided by 10.
\end{itemize}

If the result equals 0, then the number is valid.

\exercise
We need to first find the digits of a number. Define the functions
\begin{code}
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
\end{code}
|toDigits| should convert positive |Integer|s to a list of
digits. (For |0| or negative inputs, |toDigits| should return the
empty list.)  |toDigitsRev| should do the same, but with the digits
reversed.

\begin{example}
  |toDigits 1234 == [1,2,3,4]|
\end{example}

\begin{example}
  |toDigitsRev 1234 == [4,3,2,1]|
\end{example}

\begin{example}
  |toDigits 0 == []|
\end{example}

\begin{example}
  |toDigits (-17) == []|
\end{example}

\exercise
Once we have the digits in the proper order, we need to double every
other one. Define a function
\begin{code}
doubleEveryOther :: [Integer] -> [Integer]
\end{code}
Remember that |doubleEveryOther| should double every other number
\emph{beginning from the right}, that is, the second-to-last,
fourth-to-last, \dots numbers are doubled.

\begin{example}
  |doubleEveryOther [8,7,6,5] == [16,7,12,5]|  
\end{example}

\begin{example}
  |doubleEveryOther [1,2,3] == [1,4,3]|
\end{example}

\exercise
The output of |doubleEveryOther| has a mix of one-digit and two-digit
numbers. Define the function
\begin{code}
sumDigits :: [Integer] -> Integer
\end{code}
to calculate the sum of all digits.

\begin{example}
  |sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22|
\end{example}

\exercise
Define the function
\begin{code}
validate :: Integer -> Bool
\end{code}
that indicates whether an |Integer| could be a valid credit card
number. This will use all functions defined in the previous exercises.

\begin{example}
  |validate 4012888888881881 = True|
\end{example}

\begin{example}
  |validate 4012888888881882 = False|
\end{example}

\section{The Towers of Hanoi\footnote{Adapted from an assignment given
    in UPenn CIS 552, taught by Benjamin Pierce}}

\begin{center}
\includegraphics[width=5in]{images/Tower_of_Hanoi.jpeg}
\end{center}

\exercise
The \emph{towers of Hanoi} is a simple recursive game. (You've
probably seen it before; if it is not familiar, you may want to read a
little about it on the web to get some intuition.) The purpose is to
move a stack of $n$ discs from one peg to another, using a third peg
as ``temporary'' storage and respecting the constraint that no disc is
ever placed on top of a smaller one. To move $n$ discs (stacked in
increasing size) from peg $a$ to peg $b$ using peg $c$ as temporary storage,
\begin{enumerate}
  \item move $n - 1$ discs from $a$ to $c$ using $b$ as temporary storage
  \item move the top disc from $a$ to $b$
  \item move $n - 1$ discs from $c$ to $b$ using $a$ as temporary storage.
\end{enumerate}
For this exercise, define a function |hanoi| with the following type:
\begin{code}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
\end{code}
Given the number of discs and names for the three pegs, |hanoi| should
return a list of moves to be performed to move the stack of discs from
the first peg to the second.

Note that a |type| declaration, like |type Peg = String| above, makes
a \emph{type synonym}.  In this case |Peg| is declared as a synonym
for |String|, and the two names |Peg| and |String| can now be used
interchangeably.  Giving more descriptive names to types in this way
can be used to give shorter names to complicated types, or (as here)
simply to help with documentation.

\begin{example}
  |hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]|  
\end{example}

\newpage
\exercise \opt What if there are four pegs instead of three?  That is,
the goal is still to move a stack of discs from the first peg to the
last peg, without ever placing a larger disc on top of a smaller one,
but now there are two extra pegs that can be used as ``temporary''
storage instead of only one.  Write a function similar to |hanoi|
which solves this problem in as few moves as possible.

It should be possible to do it in far fewer moves than with three
pegs.  For example, with three pegs it takes $2^{15} - 1 = 32767$
moves to transfer $15$ discs.  With four pegs it can be done in $129$
moves. (See Exercise 1.17 in Graham, Knuth, and Patashnik,
\emph{Concrete Mathematics}, second ed., Addison-Wesley, 1994.)
\end{document}
