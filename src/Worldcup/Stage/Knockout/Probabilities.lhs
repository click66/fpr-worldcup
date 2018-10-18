> module Worldcup.Stage.Knockout.Probabilities where

> import Data.List (tails, sort, sortBy, groupBy, nub, partition)

> type Probability = Rational
> type Distribution a = [(a, Probability)]

__4.3 Probabilities__
___Generation of Distributions___

One can think of a "Distribution" as a container of specified elements (each element representing an "outcome") where, behind the facade of the type synonym, each element is mapped aginst a probability. It can be taken as an invariant that all probablities in a distribution should calculate to 1.

The function "coin" will produce a distribution of (obviously two) boolean values where the provided probability is taken to be the probability of "True":

> coin :: Probability -> Distribution Bool
> coin p = [(True, p), (False, 1 - p)]

The function "point" returns a one-point distribution; that is, a single possible outcome, that outcome thus having the probability of exactly 1.

> point :: a -> Distribution a
> point x = [(x, toRational 1)]

"uniform" takes a list of elements and creates a uniform distribution of them; that is, each element has an associated probability of 1 / the number of possible outcomes.

> uniform :: [a] -> Distribution a
> uniform xs = map (\x -> (x, singleProb)) xs
>   where singleProb = (1 :: Rational) / toRational (length xs)

"mapD" maps a function over the elements within a distribution.

> mapD :: (a -> b) -> Distribution a -> Distribution b
> mapD f ds = map (\(a, p) -> (f a, p)) ds

The function "choose" represents a choice between two outcomes with the probability of the first outcome specified as the first parameter. The function will return the resulting distribution.

> choose :: Probability -> a -> a -> Distribution a
> choose p u v = mapD (\b -> if b then u else v) $ coin p


___Manipulation and Computation of Distributions___

Given the task to define "distAvg", a function that, given a distribution of distributions will compute the distributed average of the overall distribution (returning a single distribution), the task can be broken down into multiple smaller steps for clarity. As the initial step, one can define a single function "weight", which will take a Probability and a Distribution and return a Distribution with each element "weighted" by the provided probability. This function utilises list comprehension to apply the multiplication to each probability in the distribution in turn.

Since a specified invariant is that all probabilities in a distribution should sum to 1, it would be prudent, one believes, to constrain "weight" as a private function so that it cannot be used elsewhere.

The "distAvg" function can then be defined as a recursive function which applies "weight" to the head element and aggregates the result with the result of "distAvg" on the tail.

> distAvg :: Distribution (Distribution a) -> Distribution a
> distAvg []          = []
> distAvg ((d, p):ds) = (weight p d) ++ distAvg ds
>   where
>     weight p d = [(a , ap * p) | (a, ap) <- d]

As the next logical step, the "simplify" function is provided to remove any inevitable duplicates that would arise. This function works by first sorting the elements and utilising the separate "simplifySorted" function on the result:

> simplifySorted :: Eq a => Distribution a -> Distribution a
> simplifySorted ((a1, p1):(a2, p2) : ds)
>     | a1 == a2  = simplifySorted ((a1, p1 + p2) : ds)
>     | otherwise = (a1, p1) : (simplifySorted ((a2, p2) : ds))
> simplifySorted (d:[]) = [d]

> simplify :: Ord a => Distribution a -> Distribution a
> simplify [] = []
> simplify d  = simplifySorted sorted
>   where
>     sorted = sortBy (\(a1, _) (a2, _) -> compare a1 a2) d

Unfortunately, this relies on the assumption that all "contained" elements in the distribution are instances of the "Ord" class, given that the employed algorithm relies on the elements being sorted. This can be remedied by sorting the elements in a different way: the "partition" function can be used to split the list into a list of pairs containing the same contained element and a list of pairs containing the remaining elements. This function can then be recursively applied. Note that this function may produce a result that is ordered slightly differently.

> simplify' :: Eq a => Distribution a -> Distribution a
> simplify' [] = []
> simplify' ((d, dp):ds) = simplifySorted $ ((d, dp):eq) ++ simplify' neq
>   where
>     (eq, neq) = partition (\(a, p) -> a == d) ds

> andThen :: Distribution a -> (a -> Distribution b) -> Distribution b
> andThen da f = distAvg $ mapD f da