module Worldcup.ResultSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.Result

spec :: Spec
spec = do
    describe "inverse" $ do

        it "inverses to expected values" $ do
            inverse Win == Loss && inverse Draw == Draw && inverse Loss == Win

    describe "points" $ do

        it "produces expected point values" $ do
            points Win == 3 && points Draw == 1 && points Loss == 0