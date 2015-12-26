module RaptrSpec where

import           Test.Tasty
import           Test.Tasty.Hspec

gcSpec = describe "Raptr" $ do

  it "runs 3 node cluster" $ do
    result <- runRaptr
    result `shouldBe` True


