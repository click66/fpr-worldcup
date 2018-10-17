module Worldcup.Stage.Knockout.Probabilities.UniformSpec (spec) where

import Data.Ratio
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Stage.Knockout.Probabilities (uniform)
import Worldcup.SpecHelpers

spec :: Spec
spec = do
    describe "uniform" $ do

        it "produces a uniform distribution summing to 1 when given a non-empty list" $ property $
            let totalProbability ((_, p):xs) = p + (totalProbability xs)
                totalProbability [] = 0
            in \(NonEmpty xs) -> (totalProbability $ uniform (xs :: [Int])) `shouldBe` 1

        it "produces a list where each probability is equal to 1 divided by the length of the supplied list" $ property $
            let isProbability (_, p) ep = p == ep
                allExpected []     ep = True
                allExpected (d:ds) ep = (isProbability d ep) && (allExpected ds ep)
            in \(NonEmpty xs) -> allExpected (uniform (xs :: [Int])) (toRational (1 % length xs))
