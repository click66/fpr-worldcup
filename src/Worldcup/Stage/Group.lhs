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

> fixtures :: [Team] -> [Fixture]
> fixtures [] = []
