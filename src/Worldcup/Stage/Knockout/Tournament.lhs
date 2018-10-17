> module Worldcup.Stage.Knockout.Tournament
>     (
>       Tournament (..)
>     ) where

This module defines the datatype "Tournament". It exists as a separate module to allow its constructors to be utilised in other internal modules (and tests) without them being exposed automatically at the higher layers without expicit import. It's a safety thing, really.

> data Tournament a = Singleton a
>                   | Match (Tournament a) (Tournament a)
>     deriving Show