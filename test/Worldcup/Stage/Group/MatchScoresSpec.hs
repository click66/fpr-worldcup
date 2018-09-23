module Worldcup.Stage.Group.MatchScoresSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Result
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "matchScores" $ do

        let teamA   = AUS
            teamB   = BEL
            fixture = (AUS, BEL)

        it "computes correct outcome for a win" $ do
            matchScores (fixture, Win) == [(teamA, 3), (teamB, 0)]

        it "computes correct outcome for a draw" $ do
            matchScores (fixture, Draw) == [(teamA, 1), (teamB, 1)]

        it "computes correct outcome for a loss" $ do
            matchScores (fixture, Loss) == [(teamA, 0), (teamB, 3)]
