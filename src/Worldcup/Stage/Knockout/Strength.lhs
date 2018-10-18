> module Worldcup.Stage.Knockout.Strength
>     (
>       strength
>     ) where

> import Worldcup.Team

> type Strength = Rational

> odds :: [(Team, Rational)]
> odds = [(ARG, 8/1), (AUS, 300/1), (BEL, 12/1), (BRA, 5/1), (COL, 50/1), (CRC, 400/1), (CRO, 25/1), (DEN , 80/1), (EGY , 250/1), (ENG, 16/1), (FRA, 11/2), (GER, 5/1), (ISL, 200/1), (IRN , 500/1), (JPN , 250/1), (KOR, 500/1), (MEX , 66/1), (MAR, 250/1), (NGA, 150/1), (PAN , 1000/1), (PER, 150/1), (POL, 40/1), (POR, 20/1), (RUS , 40/1), (KSA,1000/1),(SEN,150/1),(SRB,150/1),(ESP,15/2),(SWE,80/1), (SUI , 66/1), (TUN , 400/1), (URU , 40/1) ]

> strengths :: [ (Team, Strength) ]
> strengths = [ (t, 1/o) | (t, o) <- odds ]

The "lookup" function is utilised to create "strength", the function that returns the strength of a provided team based on the table of odds. Since "lookup" returns as "Maybe" monad, "strength" returns the unpacked value, returning a strength of 0 in the case that no odds for the team exist.

> strength :: Team -> Strength
> strength t = case lookup t strengths of
>                  Nothing -> 0
>                  Just s  -> s
