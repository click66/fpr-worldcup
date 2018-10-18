module Worldcup.Stage.Knockout.KnockoutSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.SpecHelpers
import Worldcup.Stage.Knockout (knockout)
import Worldcup.Stage.Knockout.Tournament
import Worldcup.Team

treeDepth :: Tournament a -> Int
treeDepth (Singleton _) = 1
treeDepth (Match a b)   = 1 + (max (treeDepth a) (treeDepth b))

spec :: Spec
spec = do
    describe "knockout" $ do

        it "can create a simple knockout tournament (assuming power of 2 list team list length)" $ do
            let teams = [ AUS, BEL, CRO, DEN ]
                (Match (Match (Singleton t1) (Singleton (t2))) (Match (Singleton t3) (Singleton t4))) = knockout teams
            t1 == AUS && t2 == BEL && t3 == CRO && t4 == DEN && (treeDepth $ knockout teams) == 3

        it "can create a tournament with an irregular set of teams" $ do
            let teams = [ BRA, COL, CRO, EGY, ENG, IRN, JPN, MAR, PAN, POL, RUS, SEN, SRB, ESP, SWE, TUN, URU ] -- 17
            (treeDepth $ knockout teams) == 6

        it "returns a Null tournament if given an empty list" $ do
            let check Null = True
                check _    = False
            check $ knockout []

        it "depth = ceiling (log2n + 1)" $ property $
            forAll (sublistOf (enumerateAll :: [Team])) $ \ts -> (treeDepth $ knockout ts) == (expectedDepth $ length ts)
              where expectedDepth n = (ceiling $ logBase 2 $ fromIntegral n) + 1