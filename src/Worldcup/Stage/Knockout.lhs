4 Knockout Stage
----------------

This module encapsulates the implementation of the "Knockout Stage" task.

In the codebase, this module is spread across multiple submodules. "Worldcup.Stage.Knockout" contains the main implementation of the questions, whilst supplementary functions and datatypes are kept in separate modules for clarity (those existing under the "Worldcup.Stage.Knockout.*" namespace, specifically).

> module Worldcup.Stage.Knockout
>     (
>       knockout
>     , playMatch
>     , playTournament
>     , Tournament, knockout2018
>     ) where

> import Worldcup.Stage.Knockout.Probabilities
> import Worldcup.Stage.Knockout.Strength
> import Worldcup.Stage.Knockout.Tournament
> import Worldcup.Result
> import Worldcup.Team

__4.1 Tournaments__

A function "tournament" can create a tournament from a list of provided teams. For example:

    tournament [ AUS, BEL, CRO, DEN ]

will return:

    Match ((Match (Singleton AUS) (Singleton (BEL))) (Match (Singleton CRO) (Singleton DEN)))

> tournament :: [a] -> Tournament a
> tournament ts = case reduce $ (map Singleton ts) of
>                      [] -> Null
>                      l  -> head l

The function utilises two internal functions: "pair", which constructs a list of "Match"es out of a list of "Tournament" datatypes; and "reduce", which continuously calls pair on a given list of "Tournament" datatypes until the length of the resulting list is less than or equal to 1 (i.e. we will be left with a single "Match" encompassing the entire tournament). "tournament" then simply returns the head of this reduced resultant list.

> pair :: [Tournament a] -> [Tournament a]
> pair (t1:t2:ts) = (Match t1 t2) : pair ts
> pair (t1:[])    = [t1]
> pair []         = []

> reduce :: [Tournament a] -> [Tournament a]
> reduce ts
>     | length ts <= 1 = ts
>     | otherwise      = reduce $ pair ts

Although this function deals with irregular numbers of teams (that is, a length that is not a power of 2 or even an even number), the attempt to return the "head" of the "reduced" list will trigger a Prelude exception in the case of an empty list passed. To deal with this eventuality, a third constructor "Null" has been added to the "Tournament" datatype definition (see Worldcup.Stage.Knockout.Tournament). This is a nullary data constructor which indicates an empty tournament devoid of any team. This is handled by the case expression in the body of "tournament". For reference, the original (unsafe) body prior to this extension was:

    tournament ts = head . reduce $ (map Singleton ts)

As can be seen, "tournament" actually works on any type. Given the context in which this module is used however, it makes sense, one believes, to specialise this function into a "knockout" function:

> knockout :: [Team] -> Tournament Team
> knockout = tournament

This function is the only exposed function, and thus testing is restricted to it. This is to obey the rule of YAGNI.


__4.2 Away they go!__

The function "playMatch" returns a probability Distribution of teams. The probability of team A beating team B (P) is calculated by the formula:

    P = strength(teamA) / strength(teamA) + strength(teamB)

> playMatch :: Team -> Team -> Distribution Team
> playMatch t1 t2 = choose ((strength t1) / ((strength t1) + (strength t2))) t1 t2

> playTournament :: Tournament Team -> Distribution Team
> playTournament Null          = []
> playTournament (Singleton t) = point t
> playTournament (Match (Singleton t1) (Singleton t2)) = playMatch t1 t2
> playTournament (Match (Match t1 t2) (Match t3 t4)) = playTournament (Match t1 t2) ++ playTournament (Match t3 t4)

> knockout2018 = tournament [URU,POR,FRA,ARG,BRA,MEX,BEL,JPN, ESP,RUS,CRO,DEN,SWE,SUI,COL,ENG]