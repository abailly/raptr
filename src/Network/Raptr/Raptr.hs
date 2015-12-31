-- | Core module for Raptr library.
--
-- Import this module to instantiate a @Raptr@ node member of a cluster communicating over HTTP.
module Network.Raptr.Raptr(module Network.Raptr.Server
                          ,module Network.Raptr.Types
                          ,module Network.Kontiki.Raft) where

import           Network.Kontiki.Raft
import           Network.Raptr.Server
import           Network.Raptr.Types

runRaptr :: IO Bool
runRaptr = return False


