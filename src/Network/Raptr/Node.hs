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
import qualified Data.Map                 as Map
import           Network.Kontiki.Raft

-- | An arbitrary opaque value that is stored in the logs.
type Value = ByteString

data Node m = Node { nodeId             :: NodeId
                   , nodeElectionTimer  :: Timer
                   , nodeHeartbeatTimer :: Timer
                   , nodeInput          :: Queue (Event Value)
                   , nodeOutput         :: Map.Map NodeId (Queue (Message Value))
                   , nodeLog            :: (MonadLog m Value) => m Value
                   , nodeCommitIndex    :: Index
                   }


newNode :: (MonadLog m Value) => NodeId -> Map.Map NodeId (Queue (Message Value)) -> m Value -> IO (Node m)
newNode nid channels log = do
  electionTimer <- newTimer
  heartbeatTimer <- newTimer
  q <- newQueueIO 1000

  return $  Node { nodeId = nid
                 , nodeElectionTimer =  electionTimer
                 , nodeHeartbeatTimer = heartbeatTimer
                 , nodeInput = q
                 , nodeOutput = channels
                 , nodeLog = log
                 , nodeCommitIndex = index0
                 }
