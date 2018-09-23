module Worldcup.Stage.Group.PossibleOutcomesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Result
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "possibleOutcomes" $ do

        it "produces expected number of outcomes from specific scenario" $ do
            length (possibleOutcomes [ AUS, BEL, CRO, DEN ]) == 729