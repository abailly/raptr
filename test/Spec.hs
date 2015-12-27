import           Test.Tasty
import           Test.Tasty.Hspec

import           Network.Raptr.QueueSpec

main :: IO ()
main = testGroup "Kontiki Specs" <$>
       sequence [ testSpec "Queue" queueSpec ] >>= defaultMain
