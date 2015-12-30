{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- | Defines the structure and behavior of a Raft Node, using Kontiki as model
-- implementation of Raft consensus algorithm.
module Network.Raptr.Node where

import           Control.Concurrent.Queue     as Q
import           Control.Concurrent.STM       (atomically, orElse)
import           Control.Concurrent.Timer     as Timer
import           Control.Monad                (foldM, forM_)
import           Control.Monad.State          as State
import           Control.Monad.Trans          (MonadIO, liftIO)
import qualified Data.Binary                  as B
import           Data.Binary.Put              (putWord32be, runPut)
import           Data.ByteString              (ByteString, hPut, length)
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Kontiki.MemLog          (Log, runMemLog)
import qualified Data.Map                     as Map
import           Network.Kontiki.Raft         as Raft
import           Network.Raptr.Client
import           Network.Raptr.Types
import           Network.URI                  (URI)
import           System.IO                    (IOMode (AppendMode),
                                               withBinaryFile)
import           System.IO.Storage
import           System.Random                (randomRIO)


handleCommand :: Node -> Command Value -> IO Node
handleCommand s c = case c of
    CBroadcast m -> do
        putStrLn $ "CBroadcast: " ++ show m
        doBroadcast (nodeCommunicator s) m
        return s
    CSend n m -> do
        putStrLn $ "CSend: " ++ show n ++ " -> " ++ show m
        doSend (nodeCommunicator s) n m
        return s
    CResetElectionTimeout a b -> do
        t <- randomRIO (a, b)
        putStrLn $ "Reset election timeout: " ++ show t
        Timer.reset (nodeElectionTimer s) t
        return s
    CResetHeartbeatTimeout a -> do
        putStrLn $ "Reset heartbeat timeout: " ++ show a
        Timer.reset (nodeHeartbeatTimer s) a
        return s
    CLog b -> do
        let m = LBS8.unpack $ Builder.toLazyByteString b
        putStrLn $ "Log: " ++ m
        return s
    CTruncateLog i -> do
        putStrLn $ "Truncate: " ++ show i
        -- TODO
        return s
    CLogEntries es -> do
        putStrLn $ "Log entries: " ++ show es
        let l = nodeLog s
        forM es (insertEntry l)
        return s
    CSetCommitIndex i' -> do
        let i = nodeCommitIndex s
        putStrLn $ "New commit index, to commit: " ++ entriesToCommit i i'
        return $ s { nodeCommitIndex = i' }

entriesToCommit :: Index -> Index -> String
entriesToCommit prev new =
    if | new < prev  -> error "Committed entries could not be reverted"
       | new == prev -> "nothing"
       | new == next -> "entry " ++ show new
       | otherwise   -> "entries " ++ show next ++ " to " ++ show new
  where
    next = succIndex prev

handleCommands :: Node -> [Command Value] -> IO Node
handleCommands = foldM handleCommand

data Node = Node { nodeId             :: NodeId
                 , nodeElectionTimer  :: Timer
                 , nodeHeartbeatTimer :: Timer
                 , nodeInput          :: Queue (Event Value)
                 , nodeCommunicator   :: Communicator Value
                 , nodeLog            :: FileLog
                 , nodeCommitIndex    :: Index
                 }


newtype NodeServer a = NodeServer { runServer :: StateT Node IO a }
                     deriving (Functor, Applicative, Monad, MonadState Node, MonadIO)

newNode :: NodeId -> Communicator Value -> FileLog -> IO Node
newNode nid handler log = do
  electionTimer <- newTimer
  heartbeatTimer <- newTimer
  q <- newQueueIO 100

  return $  Node { nodeId = nid
                 , nodeElectionTimer =  electionTimer
                 , nodeHeartbeatTimer = heartbeatTimer
                 , nodeInput = q
                 , nodeCommunicator = handler
                 , nodeLog = log
                 , nodeCommitIndex = index0
                 }

class (MonadIO m, MonadState Node m) => MonadCommand m where
  pollEvent :: m (Event Value)
  handleEvent :: Config -> SomeState -> Event Value -> m (SomeState, [Command Value])
  interpretCommands :: [Command Value] -> m ()

  traceS :: String -> m ()
  traceS = liftIO . putStrLn

instance MonadLog NodeServer Value where
  logEntry :: Index -> NodeServer (Maybe (Entry Value))
  logEntry idx = get >>= \ Node{..} -> liftIO (getEntry nodeLog idx)

  logLastEntry :: NodeServer (Maybe (Entry Value))
  logLastEntry = get >>= \ Node{..} -> liftIO (getLastEntry nodeLog)

instance MonadCommand NodeServer where

  pollEvent = get >>= \ Node{..} -> liftIO $ atomically $ do
    (const EElectionTimeout `fmap` Timer.awaitSTM nodeElectionTimer)
      `orElse` (const EHeartbeatTimeout `fmap` Timer.awaitSTM nodeHeartbeatTimer)
      `orElse` Q.read nodeInput

  handleEvent config state event = get >>= \ Node{..} -> Raft.handle config state event

  interpretCommands commands = get >>= liftIO . flip handleCommands commands >>= State.put


-- | Main loop for running a Raft Node.
run :: (MonadCommand m) => Config -> SomeState -> m ()
run config state = do
    event <- pollEvent
    (state', commands) <- handleEvent config state event
    interpretCommands commands
    run config state'

