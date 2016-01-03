module Network.Raptr.RaptrSpec where

import           Network.Raptr.Raptr
import           Test.Tasty
import           Test.Tasty.Hspec

raptrSpec = do

  it "runs a 3 node cluster" $ do
    let (uris, clusterConfig) = localCluster 3

    result <- runRaptr
    result `shouldBe` True


