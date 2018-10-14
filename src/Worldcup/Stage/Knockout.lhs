> module Worldcup.Stage.Knockout where

> import Data.List (tails, sort, sortBy, groupBy, nub)

> type Probability = Rational
> type Distribution a = [(a, Probability)]

The function "coin" will produce a distribution of boolean values where the provided probability is taken to be the probability of "True":

> coin :: Probability -> Distribution Bool
> coin p = [(True, p), (False, 1 - p)]

> point :: a -> Distribution a
> point x = [(x, 1)]

> uniform :: [a] -> Distribution a
> uniform xs = map (\x -> (x, singleProb)) xs
>   where singleProb = (1 :: Rational) / toRational (length xs)

> mapD :: (a -> b) -> Distribution a -> Distribution b
> mapD f ds = map (\(a, p) -> (f a, p)) ds

> choose :: Probability -> a -> a -> Distribution a
> choose p u v = mapD (\b -> if b then u else v) $ coin p