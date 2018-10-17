module Worldcup.Stage.Knockout.KnockoutSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Team
import Worldcup.Stage.Knockout (knockout)
import Worldcup.Stage.Knockout.Tournament

spec :: Spec
spec = do
    describe "knockout" $ do

        it "can create a simple knockout tournament (assuming power of 2 list team list length)" $ do
            let (Match (Match (Singleton t1) (Singleton (t2))) (Match (Singleton t3) (Singleton t4))) = knockout [ AUS, BEL, CRO, DEN ]
            t1 == AUS && t2 == BEL && t3 == CRO && t4 == DEN
