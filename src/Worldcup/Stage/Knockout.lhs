> module Worldcup.Stage.Knockout where

> import Worldcup.Stage.Knockout.Probabilities

> data Tournament a = Singleton a | Match (Tournament a) (Tournament a) deriving Show

> type Strength = Rational

