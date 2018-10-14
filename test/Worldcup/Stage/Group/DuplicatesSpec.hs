module Worldcup.Stage.Group.DuplicatesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Worldcup.SpecHelpers
import Worldcup.Stage.Group
import Worldcup.Team

spec :: Spec
spec = do
    describe "duplicates" $ do

        it "produces the expected groups from the provided scenario" $ do
            duplicates [ (1, 'a'), (2, 'b'), (3, 'b'), (5, 'a'), (6, 'c') ]
                == ([[(1, 'a'), (5, 'a')], [(2, 'b'), (3, 'b')]])
