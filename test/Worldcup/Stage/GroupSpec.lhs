> module Worldcup.Stage.GroupSpec (spec) where

> import Test.Hspec
> import Test.Hspec.QuickCheck
> import Test.QuickCheck
> import Worldcup.Stage.Group

> spec :: Spec
> spec = do 
>     describe "fixtures" $ do

>         it "returns an empty list when given no teams" $ do
>             fixtures [] `shouldBe` []

>         it "returns an empty list when given a single team" $ do
>             fixtures [ AUS ] `shouldBe` []

>         it "handles duplicates" $ do
>             let teams            = [ AUS, BEL, COL, AUS ]
>                 expectedFixtures =
>                     [ (AUS, BEL)
>                     , (AUS, COL)
>                     , (BEL, COL)
>                     ]
>             fixtures teams `shouldBe` expectedFixtures

>         it "generates expected matches from a set of four specific teams" $ do
>             let teams            = [ AUS, BEL, COL, DEN ]
>                 expectedFixtures =
>                     [ (AUS, BEL)
>                     , (AUS, COL)
>                     , (AUS, DEN)
>                     , (BEL, COL)
>                     , (BEL, DEN)
>                     , (COL, DEN)
>                     ]
>             fixtures teams `shouldBe` expectedFixtures
