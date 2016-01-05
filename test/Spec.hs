import           Test.Tasty
import           Test.Tasty.Hspec

import           Network.Raptr.QueueSpec
import           Network.Raptr.RaptrSpec
import           Network.Raptr.ServerSpec
import           Network.Raptr.StorageSpec

main :: IO ()
main = testGroup "Kontiki Specs" <$>
       sequence [ testSpec "Queue" queueSpec
                , testSpec "Raptr Server" serverSpec
                , testSpec "Raptr Storage" storageSpec
                , testSpec "Raptr Cluster" raptrSpec
                , testSpec "Raptr Client" clientSpec
                ] >>= defaultMain
