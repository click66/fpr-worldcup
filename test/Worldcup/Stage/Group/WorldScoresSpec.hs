module Worldcup.Stage.Group.WorldScoresSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Result
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "worldScores" $ do

        it "computes expected world from specific scenario" $ do
            let world    = [ ((AUS, BEL), Win), ((AUS, COL), Win), ((AUS, DEN), Win),
                             ((BEL, COL), Win), ((BEL, DEN), Win), ((COL, DEN), Win) ]
                expected = [ (AUS , 9), (BEL, 6), (COL, 3), (DEN , 0) ]
            worldScores world === expected