> module Worldcup.Stage.Knockout
>     (
>       knockout
>     , Tournament
>     ) where

> import Worldcup.Team
> import Worldcup.Stage.Knockout.Probabilities
> import Worldcup.Stage.Knockout.Tournament

> type Strength = Rational

> odds :: [(Team, Rational)]
> odds = [(ARG, 8/1), (AUS, 300/1), (BEL, 12/1), (BRA, 5/1), (COL, 50/1), (CRC, 400/1), (CRO, 25/1), (DEN , 80/1), (EGY , 250/1), (ENG, 16/1), (FRA, 11/2), (GER, 5/1), (ISL, 200/1), (IRN , 500/1), (JPN , 250/1), (KOR, 500/1), (MEX , 66/1), (MAR, 250/1), (NGA, 150/1), (PAN , 1000/1), (PER, 150/1), (POL, 40/1), (POR, 20/1), (RUS , 40/1), (KSA,1000/1),(SEN,150/1),(SRB,150/1),(ESP,15/2),(SWE,80/1), (SUI , 66/1), (TUN , 400/1), (URU , 40/1) ]

> strengths :: [ (Team, Strength) ]
> strengths = [(t,1/o) | (t,o) <- odds]


Tournaments
-----------

A function "tournament" can create a tournament from a list of provided teams. For example:

    tournament [ AUS, BEL, CRO, DEN ]

will return:

    Match ((Match (Singleton AUS) (Singleton (BEL))) (Match (Singleton CRO) (Singleton DEN)))

> tournament :: [a] -> Tournament a
> tournament ts = head $ compile [ Match (Singleton t1) (Singleton t2) | (t1, t2) <- pairs ts]
>   where
>     pairs (t1:t2:ts) = (t1, t2) : pairs ts
>     pairs []         = []
>     compile (t1:t2:ts) = (Match t1 t2) : compile ts
>     compile []       = []

Given the context in which this module is used, it makes sense, one believes, to specialise this function into a "knockout" function:

 knockout :: [Team] -> Tournament Team

> knockout = tournament

This function is the only exposed function, and thus testing is restricted to it. This is to obey the rule of YAGNI.
