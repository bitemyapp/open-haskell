Applicative functors, Part II
=============================

CIS 194 Week 11  
1 April 2012

Suggested reading:

  * [Applicative Functors](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) from Learn You a Haskell
  * [The Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)

More Applicative Examples
-------------------------

**Lists**

How about an instance of `Applicative` for lists?  There are actually
two possible instances: one that matches up the list of functions and
list of arguments elementwise (that is, it "zips" them together), and
one that combines functions and arguments in all possible ways.

First, let's write the instance that does all possible combinations.
(For reasons that will become clear next week, this is the default
instance.)  From this point of view, lists represent nondeterminism:
that is, a value of type `[a]` can be thought of as a single value
with multiple possibilities.  Then `(<*>)` corresponds to
nondeterministic function application---that is, the application of a
nondeterministic function to a nondeterministic argument.

> instance Applicative [] where
>   pure a = [a]          -- a "deterministic" value
>   [] <*> _      = []
>   (f:fs) <*> as = map f as ++ fs <*> as

Here's an example:

> names  = ["Joe", "Sara", "Mae"]
> phones = ["555-5555", "123-456-7890", "555-4321"]
>
> employees1 = Employee <$> names <*> phones

Maybe this particular example doesn't make that much sense, but it's
not hard to imagine situations where you want to combine things in all
possible ways like this.  For example, we can do nondeterministic
arithmetic like so:

> (.+) = liftA2 (+)    -- addition lifted to some Applicative context
> (.*) = liftA2 (*)    -- same for multiplication
>
> -- nondeterministic arithmetic
> n = ([4,5] .* pure 2) .+ [6,1] -- (either 4 or 5) times 2, plus either 6 or 1
>
> -- and some possibly-failing arithmetic too, just for fun
> m1 = (Just 3 .+ Just 5) .* Just 8
> m2 = (Just 3 .+ Nothing) .* Just 8

Next, let's write the instance that does elementwise combining.
First, we must answer an important question: how should we handle
lists of different lengths?  Some thought reveals that the most
sensible thing to do is to truncate the longer list to the length of
the shorter, throwing away the extra elements.  Of course there are
other possible answers: we might, for instance, extend the shorter
list by copying the last element (but then what do we do when one of
the lists is empty); or extend the shorter list with a "neutral"
element (but then we would have to require an instance of `Monoid`, or
an extra "default" argument for the application).

This decision in turn dictates how we must implement `pure`, since we
must obey the law

~~~~ {.haskell}
pure f <*> xs === f <$> xs
~~~~

Notice that the right-hand side is a list with the same length as
`xs`, formed by applying `f` to every element in `xs`.  The only way
we can make the left-hand side turn out the same... is for `pure` to
create an infinite list of copies of `f`, because we don't know in
advance how long `xs` is going to be.

We implement the instance using a `newtype` wrapper to distinguish it
from the other list instance. The standard Prelude function `zipWith`
also comes in handy.

> newtype ZipList a = ZipList { getZipList :: [a] }
>   deriving (Eq, Show, Functor)
>
> instance Applicative ZipList where
>   pure = ZipList . repeat
>   ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

An example:

> employees2 = getZipList $ Employee <$> ZipList names <*> ZipList phones

**Reader/environment**

Let's do one final example instance, for `(->) e`.  This is known as
the *reader* or *environment* applicative, since it allows "reading"
from the "environment" `e`.  Implementing the instance is not too
hard, we just have to use our nose and follow the types:

> instance Functor ((->) e) where
>   fmap = (.)
>
> instance Applicative ((->) e) where
>   pure = const
>   f <*> x = \e -> (f e) (x e)

An `Employee` example:

> data BigRecord = BR { getName         :: Name
>                     , getSSN          :: String
>                     , getSalary       :: Integer
>                     , getPhone        :: String
>                     , getLicensePlate :: String
>                     , getNumSickDays  :: Int
>                     }
>
> r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2
>
> getEmp :: BigRecord -> Employee
> getEmp = Employee <$> getName <*> getPhone
>
> exQ = getEmp r

Aside: Levels of Abstraction
----------------------------

`Functor` is a nifty tool but relatively straightforward.  At first
glance it seems like `Applicative` doesn't add that much

 <!--

Local Variables:
mode:markdown
compile-command:"mk pre"
End:

-->
