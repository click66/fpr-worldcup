> module Worldcup.Stage.Knockout where

> import Data.List (tails, sort, sortBy, groupBy, nub)

> type Probability = Rational
> type Distribution a = [(a, Probability)]

One can think of a "Distribution" as a container of specified elements (each element representing an "outcome") where, behind the facade of the type synonym, each element is mapped aginst a probability. It can be taken as an invariant that all probablities in a distribution should calculate to 1.

The function "coin" will produce a distribution of boolean values where the provided probability is taken to be the probability of "True":

> coin :: Probability -> Distribution Bool
> coin p = [(True, p), (False, 1 - p)]

The function "point" returns a one-point distribution; that is, a single possible outcome, that outcome thus having the probability of exactly 1.

> point :: a -> Distribution a
> point x = [(x, toRational 1)]

"uniform" takes a list of elements and creates a uniform distribution of them; that is, each element has an associated probability of

> uniform :: [a] -> Distribution a
> uniform xs = map (\x -> (x, singleProb)) xs
>   where singleProb = (1 :: Rational) / toRational (length xs)


"mapD" maps a function over the elements within

> mapD :: (a -> b) -> Distribution a -> Distribution b
> mapD f ds = map (\(a, p) -> (f a, p)) ds


The function "choose" represents a choice between two possibilities

> choose :: Probability -> a -> a -> Distribution a
> choose p u v = mapD (\b -> if b then u else v) $ coin p

 distAvg :: Distribution (Distribution a) -> Distribution a
