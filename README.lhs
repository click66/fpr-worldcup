Functional Programming
======================

Coursework submitted by Clark Sirl, for the award of Software Engineering.

[![Build Status](https://travis-ci.com/click66/fpr-worldcup.svg?branch=master)](https://travis-ci.com/click66/fpr-worldcup)

__Running this file__

This is a literate Haskell file. After cloning this library, this file can be run with the following command:

```bash
stack build && stack exec fpr-worldcup-exe
```

__Testing__

Build the application and run the test suite with:

```bash
stack test
```

Abstract
------

> module Main where

> import Worldcup.Stage.Group
> import Worldcup.Stage.Knockout

> main :: IO ()
> main = do putStrLn "Hello!"