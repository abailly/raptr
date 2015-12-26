import           Test.Tasty
import           Test.Tasty.Hspec

import           RaptrSpec

main :: IO ()
main = testSpec "Bake" raptrSpec >>= defaultMain
