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

The function utilises Haskell's list comprehension on the tails of the teams list. It is essentially a nested loop, iterating within each team to pair the "head" of each tail with the remaining elements in that tail. 

Any duplicate teams passed will be ignored (this is achieved by simply filtering out the duplicates before running the algorithm. In a case where it is not possible to make any pairs (e.g. the list contains only a single team), an empty list will be returned.

> fixtures :: [Team] -> [Fixture]
> fixtures []     = []
> fixtures [team] = []
> fixtures teams  = [(teamA, teamB) | (teamA:teams) <- tails uniques, teamB <- teams]
>                     where
>                       uniques = nub teams
