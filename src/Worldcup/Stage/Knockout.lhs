> module Worldcup.Stage.Knockout
>     (
>       knockout
>     , Tournament
>     ) where

> import Worldcup.Stage.Knockout.Probabilities
> import Worldcup.Stage.Knockout.Tournament
> import Worldcup.Team

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
> tournament ts = case compile $ (map Singleton ts) of
>                      [] -> Null
>                      l  -> head l

The function utilises two internal functions: "pair", which constructs a list of "Match"es out of a list of "Tournament" datatypes; and "compile", which continuously calls pair on a given list of "Tournament" datatypes until the length of the resulting list is less than or equal to 1 (i.e. we will be left with a single "Match" encompassing the entire tournament). "tournament" then simply returns the head of this compiled resultant list.

> pair :: [Tournament a] -> [Tournament a]
> pair (t1:t2:ts) = (Match t1 t2) : pair ts
> pair (t1:[])    = [t1]
> pair []         = []

> compile :: [Tournament a] -> [Tournament a]
> compile ts
>     | length ts <= 1 = ts
>     | otherwise      = compile $ pair ts

Although this function deals with irregular numbers of teams (that is, a length that is not a power of 2 or even an even number), the attempt to return the "head" of the "compiled" list will trigger a Prelude exception in the case of an empty list passed. To deal with this eventuality, a third constructor "Null" has been added to the "Tournament" datatype definition (see Worldcup.Stage.Knockout.Tournamnet). This is a nullary data constructor which indicates an empty tournament devoid of any team. This is handled by the case expression in the body of "tournament". For reference, the original (unsafe) body prior to this extension was:

    tournament ts = head . compile $ (map Singleton ts)

As can be seen, "tournament" actually works on any type. Given the context in which this module is used however, it makes sense, one believes, to specialise this function into a "knockout" function:

> knockout :: [Team] -> Tournament Team
> knockout = tournament

This function is the only exposed function, and thus testing is restricted to it. This is to obey the rule of YAGNI.
