> module Worldcup.Result where

This module defines a "Result"; in its most basic form, this is simply an algabreic datatype with three available constructors: Win, Draw and Loss:

> data Result = Win | Draw | Loss
>     deriving (Eq, Show)

However, it emerged during implementation that a "Result" datatype displays two properties in its usage. Firstly, a result encapsulates an integer value representing the number of points a team will score from the result. Secondly, each result has an "inverse" result; e.g. if one team has a "Win", there will be another team with a "Loss".

The latter property, that every result has an inverse result, is captured by the type class "Inverseable", asserting that the datatype implements the "inverse" function.

> class Inverseable a where
>     inverse :: a -> a

> instance Inverseable Result where
>     inverse Win  = Loss
>     inverse Draw = Draw
>     inverse Loss = Win

That each result encapsulates an integer value for its equivalent number of points is provided by the function "points". The function uses pattern matching on the constructor to set the points.

> points :: Result -> Int
> points Win  = 3
> points Draw = 1
> points Loss = 0
