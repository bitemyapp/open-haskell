
> {-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

Applicative functors
====================

CIS 194 Week 10
22 March 2012

Suggested reading:

  * [Applicative Functors](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) from Learn You a Haskell
  * [The Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)

Motivation
----------

Consider the following `Employee` type:

> type Name = String
>
> data Employee = Employee { name    :: Name
>                          , phone   :: String }
>                 deriving Show

Of course, the `Employee` constructor has type

~~~~ {.haskell}
Employee :: Name -> String -> Employee
~~~~

That is, if we have a `Name` and a `String`, we can apply the
`Employee` constructor to build an `Employee` object.

Suppose, however, that we don't have a `Name` and a `String`; what we
actually have is a `Maybe Name` and a `Maybe String`.  Perhaps they
came from parsing some file full of errors, or from a form where some
of the fields might have been left blank, or something of that sort.
We can't necessarily make an `Employee`.  But surely we can make a
`Maybe Employee`.  That is, we'd like to take our `(Name -> String ->
Employee)` function and turn it into a `(Maybe Name -> Maybe String ->
Maybe Employee)` function.  Can we write something with this type?

~~~~ {.haskell}
(Name -> String -> Employee) ->
(Maybe Name -> Maybe String -> Maybe Employee)
~~~~

Sure we can, and I am fully confident that you could write it in your
sleep by now.  We can imagine how it would work: if either the name or
string is `Nothing`, we get `Nothing` out; if both are `Just`, we get
out an `Employee` built using the `Employee` constructor (wrapped in
`Just`).  But let's keep going...

Consider this: now instead of a `Name` and a `String` we have a
`[Name]` and a `[String]`.  Maybe we can get an `[Employee]` out of
this?  Now we want

~~~~ {.haskell}
(Name -> String -> Employee) ->
([Name] -> [String] -> [Employee])
~~~~

We can imagine two different ways for this to work: we could match up
corresponding `Name`s and `String`s to form `Employee`s; or we could
pair up the `Name`s and `String`s in all possible ways.

Or how about this: we have an `(e -> Name)` and `(e -> String)`
for some type `e`.  For example, perhaps `e` is some huge data
structure, and we have functions telling us how to extract a `Name` and
a `String` from it.  Can we make it into an `(e -> Employee)`, that
is, a recipe for extracting an `Employee` from the same structure?

~~~~ {.haskell}
(Name -> String -> Employee) ->
((e -> Name) -> (e -> String) -> (e -> Employee))
~~~~

No problem, and this time there's really only one way to write this
function.

Generalizing
------------

Now that we've seen the usefulness of this sort of pattern, let's
generalize a bit.  The type of the function we want really looks
something like this:

~~~~ {.haskell}
(a -> b -> c) -> (f a -> f b -> f c)
~~~~

Hmm, this looks familiar... it's quite similar to the type of `fmap`!

~~~~ {.haskell}
fmap :: (a -> b) -> (f a -> f b)
~~~~

The only difference is an extra argument; we might call our desired
function `fmap2`, since it takes a function of two arguments.  Perhaps
we can write `fmap2` in terms of `fmap`, so we just need a `Functor`
constraint on `f`:

> fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
> fmap2 h fa fb = undefined

Try hard as we might, however, `Functor` does not quite give us enough
to implement `fmap2`.  What goes wrong?  We have

~~~~ {.haskell}
h  :: a -> b -> c
fa :: f a
fb :: f b
~~~~

Note that we can also write the type of `h` as `a -> (b -> c)`. So,
we have a function that takes an `a`, and we have a value of type `f
a`... the only thing we can do is use `fmap` to lift the function over
the `f`, giving us a result of type:

~~~~ {.haskell}
h         :: a -> (b -> c)
fmap h    :: f a -> f (b -> c)
fmap h fa :: f (b -> c)
~~~~

OK, so now we have something of type `f (b -> c)` and something of
type `f b`... and here's where we are stuck!  `fmap` does not help any
more.  It gives us a way to apply functions to values inside a
`Functor` context, but what we need now is to apply a functions *which
is itself in a `Functor` context* to a value in a `Functor` context.

Applicative
-----------

Functors for which this sort of "contextual application" is possible
are called *applicative*, and the `Applicative` class (defined in
[`Control.Applicative`](http://haskell.org/ghc/docs/6.12.1/html/libraries/base-4.2.0.0/Control-Applicative.html))
captures this pattern.

> class Functor f => Applicative f where
>   pure  :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

The `(<*>)` operator (often pronounced "ap", short for "apply")
encapsulates exactly this principle of "contextual application".  Note
also that the `Applicative` class requires its instances to be
instances of `Functor` as well, so we can always use `fmap` with
instances of `Applicative`.  Finally, note that `Applicative` also has
another method, `pure`, which lets us inject a value of type `a` into
a container.  For now, it is interesting to note that `fmap0` would be
another reasonable name for `pure`:

~~~~ {.haskell}
pure  :: a             -> f a
fmap  :: (a -> b)      -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
~~~~

Now that we have `(<*>)`, we can implement `fmap2`, which in the
standard library is actually called `liftA2`:

> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftA2 h fa fb = h `fmap` fa <*> fb

In fact, this pattern is so common that `Control.Applicative` defines
`(<$>)` as a synonym for `fmap`,

> (<$>) :: Functor f => (a -> b) -> f a -> f b
> (<$>) = fmap

so that we can write

~~~~ {.haskell}
liftA2 h fa fb = h <$> fa <*> fb
~~~~

What about `liftA3`?

> liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
> liftA3 h fa fb fc = h <$> fa <*> fb <*> fc

Nifty!  Unlike the jump from `fmap` to `liftA2` (which required
generalizing from `Functor` to `Applicative`), going from `liftA2` to
`liftA3` (and from there to `liftA4`, ...) requires no extra
power---`Applicative` is enough.

Actually, when we have all the arguments like this we usually don't
bother calling `liftA2`, `liftA3`, and so on, but just use the `f <$>
x <*> y <*> z <*> ...` pattern directly. (`liftA2` and friends do come
in handly for partial application, however.)

But what about `pure`?  `pure` is for situations where we want to
apply some function to arguments in the context of some functor `f`,
but one or more of the arguments is *not* in `f`---those arguments
are "pure", so to speak.  We can use `pure` to lift them up into `f`
first before applying.  Like so:

> liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
> liftX h fa b fc = h <$> fa <*> pure b <*> fc

Applicative laws
----------------

There is only one really "interesting" law for `Applicative`:

~~~~ {.haskell}
f `fmap` x === pure f <*> x
~~~~

Mapping a function `f` over a container `x` ought to give the same
results as first injecting the function into the container, and then
applying it to `x` with `(<*>)`.

There are other laws, but they are not as instructive; you can read
about them on your own if you really want.

Applicative examples
--------------------

**Maybe**

Let's try writing some instances of `Applicative`, starting with
`Maybe`.  `pure` works by injecting a value into a `Just` wrapper;
`(<*>)` is function application with possible failure.  The result is
`Nothing` if either the function or its argument are.

> instance Applicative Maybe where
>   pure              = Just
>   Nothing <*> _     = Nothing
>   _ <*> Nothing     = Nothing
>   Just f <*> Just x = Just (f x)

Let's see an example:

> m_name1, m_name2 :: Maybe Name
> m_name1 = Nothing
> m_name2 = Just "Brent"
>
> m_phone1, m_phone2 :: Maybe String
> m_phone1 = Nothing
> m_phone2 = Just "555-1234"
>
> exA = Employee <$> m_name1 <*> m_phone1
> exB = Employee <$> m_name1 <*> m_phone2
> exC = Employee <$> m_name2 <*> m_phone1
> exD = Employee <$> m_name2 <*> m_phone2

**Lists**

How about an instance for lists?  There are actually two possible
instances: one that matches up the list of functions and list of
arguments elementwise (that is, it "zips" them together), and one that
combines functions and arguments in all possible ways.

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


 <!--

Picking up from last week...

Let's try something a bit more mind-twisting:

    instance Functor ((->) e) where

What!? Well, let's follow the types: if `f = (->) e` then we want

    fmap :: (a -> b) -> (->) e a -> (->) e b

or, with `(->)` written infix:

    fmap :: (a -> b) -> (e -> a) -> (e -> b)

Hmm, this type signature seems familiar...

    instance Functor ((->) e) where
      fmap = (.)

Crazy!  What does this mean?  Well, one way to think of a value of
type `(e -> a)` is as a "`e`-indexed container" with one value of `a`
for each value of `e`.  To map a function over every value in such a
container corresponds exactly to function composition: to pick an
element out of the transformed container, we first we apply the `(e ->
a)` function to pick out an `a` from the original container, and then
apply the `(a -> b)` function to transform the element we picked.

-->


 <!--

Local Variables:
mode:markdown
compile-command:"make explec"
End:

-->
