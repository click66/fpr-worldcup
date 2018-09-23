module Worldcup.Stage.Group.DistinctOutcomesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "distinctOutcomes" $ do

        it "produces expected number of outcomes from specific scenario" $ do
            length (distinctOutcomes [ AUS, BEL, CRO, DEN ]) == 63