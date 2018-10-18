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

__Running this file__

This is a literate Haskell file which, when run, will demonstrate that the codebase functions as expected in the expected scenarios. It also demonstrates best practice for utilising this library.

After cloning this library, this file can be run with the following command:

```bash
stack build && stack exec fpr-worldcup-exe
```

__Testing__

Build the application and run the test suite with:

```bash
stack test
```

Introduction
------------

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

Group Stage
-----------

> groupStage :: IO ()
> groupStage = do
>     putStrLn "This is the group stage"

Knockout Stage
--------------

> knockoutStage :: IO ()
> knockoutStage = do
>     putStrLn "This is the knockout stage"