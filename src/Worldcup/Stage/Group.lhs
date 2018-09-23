> module Worldcup.Stage.Group
>     (
>       distinctOutcomes
>     , distinctWorldScores
>     , fixtures
>     , matchScores
>     , possibleOutcomes
>     , worlds
>     , worldScores
>     ) where

> import Data.List (tails, sort, sortBy, groupBy, nub)
> import Data.Function (on)
> import Worldcup.Result
> import Worldcup.Team

> results = [ Win, Draw, Loss ]

Fixtures
--------

A function "fixtures" generates all matches (non-repeating combinatorial pairs) for a given group of teams; that is, each team has one fixture against each of the other teams. For example, given a list of teams:

    fixtures [ AUS, BEL, COL, DEN ]

this function will return 6 fixtures:

     [ (AUS , BEL), (AUS , COL), (AUS , DEN), (BEL, COL), (BEL, DEN), (COL, DEN) ]

> type Fixture = (Team, Team)

The function utilises Haskell's list comprehension on the tails of the teams list. It is essentially a nested loop, iterating within each team to pair the "head" of each tail with the remaining elements in that tail.

Any duplicate teams passed will be ignored (this is achieved by simply filtering out the duplicates before running the algorithm. In a case where it is not possible to make any pairs (e.g. the list contains only a single team), an empty list will be returned.

> fixtures :: [Team] -> [Fixture]
> fixtures []     = []
> fixtures [team] = []
> fixtures teams  = [(teamA, teamB) | (teamA:teams) <- tails uniques, teamB <- teams]
>                     where uniques = nub teams


Worlds
------

A function "worlds" generates all possible worlds (a "world" being a collection of fixtures together with their results) from a given group of fixtures.

> type World   = [(Fixture, Result)]

This is achieved first through the definition of the private function "resultSets". This function, given a list of possible results will produce a list of all possible permutations of these results across a given size. e.g.:

    resultSets 3 [ Win, Loss, Draw ]

will produce a list of sets of results such as (to show a sample, order may practically differ):

    [
        [ Win, Win, Win ],    [ Win, Win, Draw ],   [ Win, Draw, Win ], [ Draw, Win, Win ],
        [ Win, Win, Loss ],   [ Win, Loss, Win ],   [ Loss, Win, Win ], [ Loss, Win, Draw],
        ...
    ]

> resultSets :: Int -> [Result] -> [[Result]]
> resultSets n results = (sequence . replicate n) results

Each result permutation generated by this function will then be `zip`ed onto the provided set of fixtures, to produce a list of (Fixture, Result) tuples. This logic is encapsulated into the "worlds" function:

> worlds :: [Fixture] -> [World]
> worlds [] = [[]]
> worlds fs = map (zip fs) (resultSets (length fs) results)

Given the above-defined "worlds" and "fixtures" functions, it is possible to create a function that translates directly from a list of teams to a list of worlds through function composition:

    teamsToWorlds = worlds . fixtures


Match Scores
------------

A function "matchScores" translates a (Fixture, Result) tuple into a list of scores (a score being a tuple of (Team, Int), the Int value being that of the number of scored points):

> type Score = (Team, Int)

One option for implementing this function would be with simple guards for each possible Win, such as:

    matchScores ((teamA, teamB), r)
        | r == Win  = [(teamA, 3), (teamB, 0)]
        | r == Loss = [(teamA, 0), (teamB, 3)]
        | r == Draw = [(teamA, 1), (teamB, 1)]

However, for reasons of cleanliness, the DRY principle and extensibility, this logic has been abstracted out to the "points" and "inverse" functions exposed with the Result data type (see the Result module for more information). This allows a one-line function definition:

> matchScores :: (Fixture, Result) -> [Score]
> matchScores ((teamA, teamB), r) = [(teamA, points r), (teamB, points $ inverse r)]

It is worth mentioning that initially this function was implemented using the guards, but was later refactored. The simple implementation helped with the creation of the tests which then gave one confidence in the ability to refactor.

World Scores
------------

Thus a function "worldScores", given a world, will compute all final scores for all teams in the world. The result is a list of scores, e.g.:

    [ (AUS, 9), (BEL, 6), (COL, 3), (DEN, 0) ]

> worldScores :: World -> [Score]
> worldScores w = map groupTotal groupedScores
>   where
>     individualScores  = concat $ map matchScores w
>     groupedScores     = (groupBy (\s1 s2 -> fst s1 == fst s2) . sort) individualScores
>     groupTotal (x:xs) = foldr (\x y -> (fst x, snd x + snd y)) x xs

This function works in several steps. First, `individualScores` maps the previously defined `matchScores` function over the (Fixture, Result) pairs in the given world, giving a list of lists of pairs of scores. "concat" is then used to flatten this into a list of scores, with duplicated teams.

Secondly, `groupedScores` uses a composed "groupBy" and "sort" to create a list of lists of scores, with each list containing exclusively the scores of a particular team.

Finally, the `groupTotal` function performs a "foldr" to sum all point values in a list of scores, utilising the head as the initial value for the "accumulator". This function is then mapped over the list of lists, producing the final list of non-duplicated scores.


Outcomes
--------

From the above, it would be possible to compute a list of all possible score combinations for all possible worlds in a group, using an expression such as:

    [worldScores world | world <- (worlds . fixtures) [ AUS, BEL, CRO, DEN ]]

This logic is exposed by the function "possibleOutcomes":

> possibleOutcomes :: [Team] -> [(World, [Score])]
> possibleOutcomes teams = [(world, worldScores world) | world <- (worlds . fixtures) teams ]

Of course, whilst this function utilises list comprehension, this is a simple case that could equally be achieved using a map and a lambda. However, it is one's opinion that the the list comprehension expression is simply cleaner:

    possibleOutcomes' :: [Team] -> [(World, [Score])]
    possibleOutcomes' teams = map (\world -> (world, worldScores world)) ((worlds . fixtures) teams)


Distinct Outcomes
-----------------

Given that many of these worlds are the same, and therefore unenlightening, it is prudent to create the capability to generate distinct outcomes of a group; that is, outcomes which produce a distinct set of scores. The defined predicate "decreasingScores" determines if a given set of scores are exclusively "decreasing" (that is, each score is equal to or less than its preceding score).

> decreasingScores :: [Score] -> Bool
> decreasingScores []     = True
> decreasingScores [s]    = True
> decreasingScores (x:xs)
>     | x `lessThan` head xs = False
>     | otherwise            = decreasingScores xs
>   where lessThan (t1, s1) (t2, s2) = s1 < s2

In order to allow this predicate to be used with the output of the "possibleOutcomes" function, an alternate implementation, "decreasingScores'" will be utilised:

> decreasingScores' :: (World, [Score]) -> Bool
> decreasingScores' (world, scores) = decreasingScores scores

Thus the function "distinctOutcomes" can be created with a simple composition:

> distinctOutcomes :: [Team] -> [(World, [Score])]
> distinctOutcomes = filter decreasingScores' . possibleOutcomes

Just for fun, the same idea can be applied using the original "decreasingScores" implementation and the earlier-defined "fixtures", "worlds" and "worldScores" functions, in order to create a "distinctWorldScores":

> distinctWorldScores :: [Team] -> [[Score]]
> distinctWorldScores = filter decreasingScores . map worldScores . worlds . fixtures