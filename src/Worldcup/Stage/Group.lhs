> module Worldcup.Stage.Group
>     (
>       Team (..)
>     , fixtures
>     ) where

> import Data.List (tails, sort, sortBy, groupBy, nub)
> import Data.Function (on)

> data Team = ARG | AUS | BEL | BRA | COL | CRC | CRO | DEN
>           | EGY | ENG | FRA | GER | ISL | IRN | JPN | KOR
>           | MEX | MAR | NGA | PAN | PER | POL | POR | RUS
>           | KSA | SEN | SRB | ESP | SWE | SUI | TUN | URU
>     deriving (Eq, Ord, Enum, Show)

> data Result = Win | Draw | Loss
>     deriving (Eq, Show)

> results = [ Win, Draw, Loss ]

> type Fixture = (Team, Team)
> type World   = [(Fixture, Result)]


A function "fixtures" generates all matches (non-repeating combinatorial pairs) for a given group of teams; that is, each team has one fixture against each of the other teams. For example, given a list of teams:

    fixtures [ AUS, BEL, COL, DEN ]

this function will return 6 fixtures:

     [ (AUS , BEL), (AUS , COL), (AUS , DEN), (BEL, COL), (BEL, DEN), (COL, DEN) ]

> fixtures :: [Team] -> [Fixture]
> fixtures []     = []
> fixtures [team] = []
> fixtures l      = [(x, y) | (x:ys) <- tails uniques, y <- ys]
>                     where
>                       uniques = nub l
