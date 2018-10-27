Functional Programming
======================

Coursework submitted by Clark Sirl, for the award of Software Engineering.

[![Build Status](https://travis-ci.com/click66/fpr-worldcup.svg?branch=master)](https://travis-ci.com/click66/fpr-worldcup)

Abstract
--------

This codebase serves as a demonstration of functional programming with the Haskell programming language, an elegant, pure functional programming language. Herein the reader will find various practiced techniques utilising features of the language, including:

* Conditionals with pattern-matching, guards, and case and if expressions
* Higher-order function manipulation, including partial application and composition
* Polymorphism
* Recursion
* Algabreic Data Types and the use of Type Classes
* Monadic computation
* Property-based testing using the HUnit and QuickCheck libraries

The codebase is structured in a modular format with the goal of enabling good testing coverage whilst also exposing no more than what is required at each layer, thus demonstrating good code encapsulation.

__Report structure__

The report is structured traditionally in four sections. Sections 1 and 2 encapsulate the demonstration code which, when ran, will perform a demonstration of the library to show that it functions as expected.

Section 3 details the implementation behind the "Group Stage" part of the assignment. This is included in the Worldcup.Stage.Group module (filepath: src/Worldcup/Stage/Group.lhs).

Section 4 deatils the implementation behind the "Knockout Stage" part of the assignment. This is included in the Worldcup.Stage.Knockout module, which is defined in the file src/Worldcup/Stage/Knockout.lhs, with some smaller parts split into individual files within the src/Worldcup/Stage/Knockout directory for cleanliness.

__Running this file__

This is a literate Haskell file which, when run, will demonstrate that the codebase functions as expected in the expected scenarios. It also demonstrates best practice for utilising this library. This is encapsulated in sections 1 and 2.

After cloning this library, this file can be run with the following command:

```bash
stack build && stack exec fpr-worldcup-exe
```

__Testing__

Build the application and run the test suite with:

```bash
stack test
```

Contents
--------

The documentation is spread across various modules and submodules in the codebase. This table of contents provides a reference for the locations of the sections that encompass the entire implementation.

|     | File | Line no. |
| --- | --- | --- |
| 1 Introduction              | README.lhs | 77
| 2 Demonstration             | | 101
| - 2.1 Group Stage           | | 106
| - 2.2 Knockout Stage        | | 159
| 3 Group Stage               | src/Worldcup/Stage/Group.lhs | 1
| - 3.1 Fixtures              | | 26
| - 3.2 Worlds                | | 49
| - 3.3 Match Scores          | | 81
| - 3.4 World Scores          | | 101
| - 3.5 Outcomes              | | 121
| - 3.6 Distinct Outcomes     | | 138
| - 3.7 Duplicates            | | 166
| 4 Knockout Stage            | src/Worldcup/Stage/Knockout.lhs | 1
| - 4.1 Tournaments           | | 22
| - 4.2 Away they go!         | | 61
| - 4.3 "Tournament" datatype | src/Worldcup/Stage/Knockout/Tournament.lhs | 1
| - 4.4 Strengths             | src/Worldcup/Stage/Knockout/Strength.lhs | 1
| - 4.5 Probabilities         | src/Worldcup/Stage/Knockout/Probabilities.lhs | 1
| 5 The "Result" Definition   | src/Worldcup/Result.lhs | 1
| 6 The "Team" Definition     | src/Worldcup/Team.lhs | 1

1 Introduction
--------------

This file is acting as the main entrypoint for the demo application, and thus needs to be defined as the "Main" module:

> module Main where

Although internally this library utilises several functions from the standard libraries, the goal with the modular layout of this codebase is to allow the presented tasks and challenges to be achieved without importing any more than is necessary. For this library, the first things that need to be imported are the basic algabreic data types and type classes:

> import Worldcup.Result
> import Worldcup.Team

Additionally, one also needs to import the two modules which encapsulate the two main stages of the tournament that will be covered by this demonstration:

> import Worldcup.Stage.Group
> import Worldcup.Stage.Knockout

Finally, the "main" method actually acts as the entrypoint for the demonstration app:

> main :: IO ()
> main = do
>     groupStage
>     knockoutStage

2 Demonstration
---------------

This section demonstrates how the implemented functions are utilised to complete the presented tasks. Sections 3 and 4 will go into further detail about the implementations themselves.

__2.1 Group Stage__

> groupStage :: IO ()
> groupStage = do
>     putStrLn "Starting group stage..."
>     let teams = [ AUS, BEL, COL, DEN ]

Imported from the Worldcup.Team module is the enumerated data type "Team", which includes a constructor for every available team. The Worldcup.Stage.Group module exposes the "fixtures" function which, when given a list of teams, will generate all of the matches for the supplied teams in the group stage.

>     let fs = fixtures teams

A possible world consists of a collection of fixtures together with their results. All possible worlds existing with the presented scenario, in which the teams Australia, Belgium, Columbia and Denmark represented a single group, can be generated by a single call to "worlds". The function "worlds" has been defined here to actually take an argument of a list of fixtures, and so the desired worlds can be generated with:

>     let ws = worlds fs

However, as described in section 3.2, it is possible use function composition to define a function that generates worlds directly from teams:

>     let teamsToWorlds      = worlds . fixtures
>         noOfPossibleWorlds = length $ teamsToWorlds teams
>     putStrLn $ "There are " ++ (show noOfPossibleWorlds) ++ " possible worlds from teams " ++ (show teams)

The output of the above code should show a total of 729 possible worlds.

The function "possibleOutcomes" (created internally using functions "matchScores" and "worldScores") generates every possible outcome for the group. Once again, this should yeild 729 possible outcomes (each corresponding to one previous world).

>     let os = possibleOutcomes teams
>         w  = head os
>     putStrLn $ "There are " ++ (show $ length os) ++ " possible outcomes from teams " ++ (show teams)
>     putStrLn $ "In this definition, the first element of the resulting list is the world: " ++ (show w)

Internally the predicate "decreasingScores" (not exposed) allows the creation of the function "distinctOutcomes", which yeilds 63 distinct worlds:

>     let dws = distinctOutcomes teams
>     putStrLn $ "There are " ++ (show $ length dws) ++ " distinct worlds from teams " ++ (show teams)

However, some point totals occur in multiple ways. These are the worlds in which one is interested. The function "duplicates" takes a list of (a, b) pairs and extracts clusters in which the b component appears more than once. For example:

>     let ds = duplicates [ (1, 'a'), (2, 'b'), (3, 'b'), (5, 'a'), (6, 'c') ]
>     putStrLn $ "Demonstration of \"duplicates\" function: " ++ (show ds)

will yield:

  [[(1,'a'),(5,'a')],[(2,'b'),(3,'b')]]

This equips one to answer the original question, and to be able to define all duplicate outcomes that will occur given a set of teams. Given the above set of teams, this will yield 15 duplicate outcomes:

>     let dos = duplicateOutcomes teams
>     putStrLn $ "There are " ++ (show $ length os) ++ " duplicate outcomes from teams " ++ (show teams)
>     putStrLn $ "In this definition, the first ambiguous result found is: " ++ (show $ head dos)
>     putStrLn $ "The most ambiguous result, occuring in 6 distinct ways, is: " ++ (show $ map length $ duplicateOutcomes [ AUS , BEL, COL, DEN ])

The implementations of the group stage is detailed in Section 2.1

__2.2 Knockout Stage__

> knockoutStage :: IO ()
> knockoutStage = do
>     putStrLn "Starting knockout stage..."

The exposed function "knockout" (internally defined as a specialisation of the polymorphic function "tournament") takes a list of teams an constructs a knockout tournament for these teams. For example:

>     let knockout2018 = knockout [ URU, POR, FRA, ARG, BRA, MEX, BEL, JPN, ESP, RUS, CRO, DEN, SWE, SUI, COL, ENG ]
>     putStrLn $ "The generated knockout tournament for the 2018 World Cup was: " ++ (show knockout2018)

Passing this tournament into the "playTournament" function generates a complete simulation of the 2018 World Cup:

>     putStrLn $ "And this plays out as ... " ++ (show $ playTournament knockout2018)