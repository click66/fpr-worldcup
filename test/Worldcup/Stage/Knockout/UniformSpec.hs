module Worldcup.Stage.Knockout.UniformSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Stage.Knockout (uniform)

spec :: Spec
spec = do
    describe "uniform" $ do

        it "produces a uniform distribution summing to 1 when given a non-empty list" $ property $
            let totalProbability ((_, p):xs) = p + (totalProbability xs)
                totalProbability [] = 0
            in (\(NonEmpty xs) -> (totalProbability $ uniform (xs :: [Int])) `shouldBe` 1)
