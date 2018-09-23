module Worldcup.Stage.Group.WorldsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "worlds" $ do

        let exampleFixtures = [ (AUS, BEL)
                              , (AUS, COL)
                              , (AUS, DEN)
                              , (BEL, COL)
                              , (BEL, DEN)
                              , (COL, DEN)
                              ]

        it "returns a singleton list when given no fixtures" $ do
            worlds [] `shouldBe` [[]]

        it "is of length r^f where r = number of possible results & f = number of fixtures" $ property $
            forAll (sublistOf exampleFixtures) $ \fs -> (length $ worlds fs) == 3^(length fs)

        it "contains lists of length equal to length of input list" $ property $
            forAll (sublistOf exampleFixtures) $ \fs -> allEqual (map length (worlds fs))


    describe "integration with fixtures" $ do

        it "can be composed with fixtures to derive worlds from teams" $ do
            let teams = [ AUS, BEL, COL, DEN ]
                expectedWorldCount = 729
            length ((worlds . fixtures) teams) == expectedWorldCount