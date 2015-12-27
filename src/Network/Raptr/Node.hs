{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | Defines the structure and behavior of a Raft Node, using Kontiki as model
-- implementation of Raft consensus algorithm.
module Network.Raptr.Node where

import           Control.Concurrent.Queue
import           Control.Concurrent.Timer
import           Data.ByteString          (ByteString)
import           Data.Kontiki.MemLog      (Log, runMemLog)
import           Network.Kontiki.Raft

-- | An arbitrary opaque value that is stored in the logs.
type Value = ByteString

data Node m = Node { nodeId             :: NodeId
                   , nodeElectionTimer  :: Timer
                   , nodeHeartbeatTimer :: Timer
                   , nodeInput          :: Queue (Event Value)
                   , nodeOutput         :: Queue (Message Value)
                   , nodeLog            :: (MonadLog m Value) => m Value
                   , nodeCommitIndex    :: Index
                   }


