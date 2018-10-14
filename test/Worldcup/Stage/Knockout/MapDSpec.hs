module Worldcup.Stage.Knockout.MapDSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Stage.Knockout (coin, mapD)

spec :: Spec
spec = do
    describe "mapD" $ do

        it "translates to integer" $ do
            expected (mapD (\b -> if b then 1 else 0) (coin $ toRational 0.5))
          where
            expected [(_, p1), (_, p2)] = p1 == 0.5 && p2 == 0.5
