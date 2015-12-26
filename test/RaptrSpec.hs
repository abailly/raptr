module RaptrSpec where

import           Network.Raptr.Raptr
import           Test.Tasty
import           Test.Tasty.Hspec

raptrSpec = describe "Raptr" $ do

  it "runs 3 node cluster" $ do
    result <- runRaptr
    result `shouldBe` True


