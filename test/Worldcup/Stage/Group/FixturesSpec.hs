module Worldcup.Stage.Group.FixturesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "fixtures" $ do

        it "returns an empty list when given no teams" $ do
            fixtures [] `shouldBe` []

        it "returns an empty list when given a single team" $ do
            fixtures [ AUS ] `shouldBe` []

        it "handles duplicates" $ do
            let teams            = [ AUS, BEL, COL, AUS ]
                expectedFixtures =
                    [ (AUS, BEL)
                    , (AUS, COL)
                    , (BEL, COL)
                    ]
            fixtures teams `shouldBe` expectedFixtures

        it "generates expected matches from a set of four specific teams" $ do
            let teams            = [ AUS, BEL, COL, DEN ]
                expectedFixtures =
                    [ (AUS, BEL)
                    , (AUS, COL)
                    , (AUS, DEN)
                    , (BEL, COL)
                    , (BEL, DEN)
                    , (COL, DEN)
                    ]
            fixtures teams `shouldBe` expectedFixtures

-- The function `((n(n-1))/2` tells us the expected number of fixtures from a set of teams, where n is the length of the set of teams. The `sublistOf` QuickCheck function allows QuickCheck to assert this as a property against various subsets of all possible teams.

        it "is of length ((n(n-1))/2) where n is the length of the input list" $ property $
            forAll (sublistOf (enumerateAll :: [Team])) $ \ts -> (length $ fixtures ts) == expectedLength (length ts)
              where expectedLength n = (n * (n-1)) `div` 2
