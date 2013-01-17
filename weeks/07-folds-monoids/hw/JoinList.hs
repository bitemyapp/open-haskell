{-# LANGUAGE MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

-- CIS 194, Fall 2010
-- HW 5 sample solution

module JoinList where

import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

-- Just for convenience. Hooray, it's in Data.Monoid!
{-
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
-}

-- Basic joinlist type.
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------
-- Exercise 1 --
----------------

-- Get the tag at the root of a joinlist.
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ l2    = l2
l1    +++ Empty = l1
l1    +++ l2    = Append (tag l1 <> tag l2) l1 l2

----------------
-- Exercise 2 --
----------------

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ 0 (Single _ a)   = Just a
indexJ _ (Single _ _)   = Nothing
indexJ n (Append _ l1 l2)
    | n < sz_l1 = indexJ n l1
    | otherwise = indexJ (n - sz_l1) l2
    where sz_l1 = jlSize l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ n l | n <= 0   = l
dropJ n (Single _ _) = Empty
dropJ n (Append _ l1 l2)
  | n < sz_l1         = dropJ n l1 +++ l2
  | otherwise         = dropJ (n - sz_l1) l2
  where sz_l1 = jlSize l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ n l | n <= 0     = Empty
takeJ n s@(Single _ _) = s
takeJ n (Append _ l1 l2)
  | n <= sz_l1          = takeJ n l1
  | otherwise           = l1 +++ takeJ (n - sz_l1) l2
  where sz_l1 = jlSize l1

-- Notice that indexJ, dropJ, and takeJ look quite similar.  Is there
-- any common pattern we can abstract out?  Indeed there is, but it's
-- tricky.  The following is just for fun; we weren't expecting you to
-- come up with this!

-- splitJ is the central function.  Given a predicate (a function
-- returning Bool) on the annotations, we search through the joinlist
-- for the place where the predicate first changes from False to True,
-- and return the joinlist split into two pieces: the part for which
-- the predicate was False, and the part for which it was True. Note,
-- however, that we don't look at the annotations directly but at the
-- *cumulative* annotation obtained by mappend'ing all the annotations
-- from the beginning of the joinlist up to the current point.
--
-- To ensure that the predicate switches from False to True exactly
-- once, the predicate must be *monotonic*, that is, for any x and y,
-- (p x) implies (p (x <> y)): in other words, once we have found an x
-- for which the predicate is True, mappending other elements onto x
-- will never make the predicate become False again.  Fortunately,
-- many useful predicates on annotations satisfy this property.
splitJ :: Monoid m => (m -> Bool) -> JoinList m a -> (JoinList m a, JoinList m a)
splitJ = splitJ' mempty -- start with an accumulator of mempty
  where
    -- if the predicate is true for the current accumulated
    -- annotation, then it will be true for the rest of the joinlist
    -- (by monotonicity).
    splitJ' m p jl | p m = (Empty, jl)

    -- the empty joinlist splits trivially.
    splitJ' _ _ Empty = (Empty, Empty)

    -- For a Single node, check whether the predicate gives True or
    -- False to decide which side to put it on.
    splitJ' m p jl@(Single m1 a)
      | p (m <> m1) = (Empty, jl)
      | otherwise   = (jl, Empty)

    -- For an Append node,
    splitJ' m p jl@(Append m1 jl1 jl2)
        -- the predicate is never satisfied
      | not $ p (m <> m1) = (jl, Empty)

        -- the predicate becomes satisfied somewhere inside the left branch,
        -- so recursively split it
      | p (m <> tag jl1)  = (jl1_l, jl1_r +++ jl2)

        -- else recursively split the right branch
      | otherwise         = (jl1 +++ jl2_l, jl2_r)

      where (jl1_l, jl1_r) = splitJ' m p jl1
            (jl2_l, jl2_r) = splitJ' (m <> tag jl1) p jl2

-- Now that we have splitJ, writing indexJ, dropJ, and takeJ becomes
-- super-easy!

-- To drop n elements, we search for the first place where the Size
-- becomes bigger than n, and take the right-hand split.  This
-- predicate is monotonic assuming that we do not allow negative Sizes
-- (which seems a reasonable assumption), since combining a Size value
-- with more Size values can only make it increase, and hence once it
-- gets above n it will always stay above n.
dropJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ' n = snd . splitJ ((>n) . getSize . size)

-- To take n elements, we do the same thing and take the left-hand split.
takeJ' n = fst . splitJ ((>n) . getSize . size)

-- First, a helper function for indexJ:

-- Split out the first element in a joinlist, if there is one.
viewL :: Monoid m => JoinList m a -> (Maybe a, JoinList m a)
viewL Empty              = (Nothing, Empty)
viewL (Single _ a)       = (Just a,  Empty)
viewL (Append _ jl1 jl2) = (a, jl1' +++ jl2)
  where (a, jl1') = viewL jl1

-- To find index n, we can simply call dropJ and take the first
-- element of the result.

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' n | n < 0 = const Nothing
indexJ' n = fst . viewL . dropJ' n

-- For more fun, try using splitJ and a Max monoid (where mappend =
-- max, and mempty is a special value representing negative infinity)
-- to implement priority queues with an extractMax function (the
-- annotation on a Single node is the priority of the element stored
-- there, so Append nodes cache the maximum priority to be found
-- anywhere below them).

----------------
-- Exercise 3 --
----------------

-- See Scrabble.hs

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

----------------
-- Exercise 4 --
----------------

toList :: JoinList m a -> [a]
toList Empty            = []
toList (Single _ a)     = [a]
toList (Append _ l1 l2) = toList l1 ++ toList l2

instance Buffer (JoinList (Score, Size) String) where
    toString      = init . unlines . toList
    fromString    = foldr (\l acc -> toSingle l +++ acc) Empty . lines
        where toSingle x = Single (scoreString x, 1) x
    line          = indexJ
    replaceLine n ln old = let prefix = takeJ n old
                               suffix = dropJ (n+1) old
                           in prefix +++ fromString ln +++ suffix
    numLines      = getSize . snd . tag
    value         = getScore . fst . tag


-- main
    
main :: IO ()
main = runEditor editor initialBuffer

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString . unlines $
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ]



-- testing

-- Class of things that can be "measured" in some way.
class Monoid m => Measured a m where
  measure :: a -> m

instance (Measured a m1, Measured a m2) => Measured a (m1,m2) where
  measure a = (measure a, measure a)

instance Measured a Size where
  measure _ = Size 1

sg :: Measured a m => a -> JoinList m a
sg a = Single (measure a) a

t1 :: Measured Int m => JoinList m Int
t1 = (sg 3 +++ sg 5) +++ (sg 1 +++ sg 2 +++ sg 7) +++ (sg 9 +++ sg 10)

t1' :: JoinList Size Int
t1' = t1
