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
import           Control.Concurrent.STM       (TVar, atomically, newTVarIO,
                                               orElse, swapTVar)
import           Control.Concurrent.Timer     as Timer
import           Control.Monad                (foldM, forM_)
import           Control.Monad.Reader         as Read
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


handleCommand :: (MonadCommand m) => Node -> Command Value -> m ()
handleCommand s c = case c of
    CBroadcast m -> do
        traceS $ "CBroadcast: " ++ show m
        liftIO $ doBroadcast (nodeClient s) m
    CSend n m -> do
        traceS $ "CSend: " ++ show n ++ " -> " ++ show m
        liftIO $ doSend (nodeClient s) n m
    CResetElectionTimeout a b -> do
        t <- liftIO $ randomRIO (a, b)
        traceS $ "Reset election timeout: " ++ show t
        liftIO $ Timer.reset (nodeElectionTimer s) t
    CResetHeartbeatTimeout a -> do
        traceS $ "Reset heartbeat timeout: " ++ show a
        liftIO $ Timer.reset (nodeHeartbeatTimer s) a
    CLog b -> do
        let m = LBS8.unpack $ Builder.toLazyByteString b
        traceS $ "Log: " ++ m
    CTruncateLog i -> do
        traceS $ "Truncate: " ++ show i
        fail "not implemented"
    CLogEntries es -> do
        traceS $ "Log entries: " ++ show es
        let l = nodeLog s
        forM_ es (liftIO . insertEntry l)
    CSetCommitIndex i' -> do
        i <- liftIO $ atomically $ swapTVar (nodeCommitIndex s) i'
        traceS $ "New commit index, to commit: " ++ entriesToCommit i i'

entriesToCommit :: Index -> Index -> String
entriesToCommit prev new =
    if | new < prev  -> error "Committed entries could not be reverted"
       | new == prev -> "nothing"
       | new == next -> "entry " ++ show new
       | otherwise   -> "entries " ++ show next ++ " to " ++ show new
  where
    next = succIndex prev

handleCommands :: (MonadCommand m) => Node -> [Command Value] -> m ()
handleCommands = mapM_ . handleCommand

data Node = Node { nodeId             :: NodeId
                 , nodeElectionTimer  :: Timer
                 , nodeHeartbeatTimer :: Timer
                 , nodeInput          :: Queue (Event Value)
                 , nodeClient         :: Client Value
                 , nodeLog            :: FileLog
                 , nodeCommitIndex    :: TVar Index
                 }


newtype NodeServer a = NodeServer { runServer :: ReaderT Node IO a }
                     deriving (Functor, Applicative, Monad, MonadReader Node, MonadIO)

newNode :: Maybe (Queue (Event Value)) -> Config -> Client Value -> FileLog -> IO Node
newNode maybeQ config handler log = do
  electionTimer <- newTimer
  heartbeatTimer <- newTimer
  q <- maybe (newQueueIO 100) return maybeQ
  idx <- newTVarIO index0
  start electionTimer (_configElectionTimeout config)
  start heartbeatTimer (_configHeartbeatTimeout config)

  return $  Node { nodeId = _configNodeId config
                 , nodeElectionTimer =  electionTimer
                 , nodeHeartbeatTimer = heartbeatTimer
                 , nodeInput = q
                 , nodeClient = handler
                 , nodeLog = log
                 , nodeCommitIndex = idx
                 }

class (MonadIO m, MonadReader Node m) => MonadCommand m where
  pollEvent :: m (Event Value)
  handleEvent :: Config -> SomeState -> Event Value -> m (SomeState, [Command Value])
  interpretCommands :: [Command Value] -> m ()

  traceS :: String -> m ()
  traceS = liftIO . putStrLn

instance MonadLog NodeServer Value where
  logEntry :: Index -> NodeServer (Maybe (Entry Value))
  logEntry idx = traceS ("get log entry at index " ++ show idx) >>
                 ask >>= \ Node{..} -> liftIO (getEntry nodeLog idx)

  logLastEntry :: NodeServer (Maybe (Entry Value))
  logLastEntry = traceS ("get last log entry") >>
                 ask >>= \ Node{..} -> liftIO (getLastEntry nodeLog)

instance MonadCommand NodeServer where

  pollEvent = traceS "polling event" >>
              ask >>= \ Node{..} -> liftIO $ atomically $ do
                (const EElectionTimeout `fmap` Timer.awaitSTM nodeElectionTimer)
                  `orElse` (const EHeartbeatTimeout `fmap` Timer.awaitSTM nodeHeartbeatTimer)
                  `orElse` Q.read nodeInput

  handleEvent config state event = traceS (show (_configNodeId config) ++ ", handling event " ++ show event ++ ", state: "++ show state) >>
                                   ask >>= \ Node{..} -> Raft.handle config state event

  interpretCommands commands = traceS ("interpreting commands " ++ show commands) >>
                               ask >>= flip handleCommands commands


-- | Main loop for running a Raft Node.
run :: (MonadCommand m) => Config -> SomeState -> m ()
run config state = do
    event <- pollEvent
    (state', commands) <- handleEvent config state event
    interpretCommands commands
    run config state'

