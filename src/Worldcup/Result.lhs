> module Worldcup.Result where

> data Result = Win | Draw | Loss
>     deriving (Eq, Show)

> class Inverseable a where
>     inverse :: a -> a

> instance Inverseable Result where
>     inverse Win  = Loss
>     inverse Draw = Draw
>     inverse Loss = Win

> points :: Result -> Int
> points Win  = 3
> points Draw = 1
> points Loss = 0
