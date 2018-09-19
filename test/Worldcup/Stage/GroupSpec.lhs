> module Worldcup.Stage.GroupSpec (spec) where

> import Test.Hspec
> import Test.Hspec.QuickCheck
> import Test.QuickCheck
> import Worldcup.Stage.Group

> spec :: Spec
> spec = do 
>     describe "fixtures" $ do
>         prop "returns an empty list when given no teams" $ do
>             fixtures [] == []
