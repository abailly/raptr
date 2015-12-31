import           Test.Tasty
import           Test.Tasty.Hspec

import           Network.Raptr.QueueSpec
import           Network.Raptr.ServerSpec

main :: IO ()
main = testGroup "Kontiki Specs" <$>
       sequence [ testSpec "Queue" queueSpec
                , testSpec "Raptr Server" serverSpec
                , testSpec "Raptr Client" clientSpec
                ] >>= defaultMain
