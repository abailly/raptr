{-# LANGUAGE RecordWildCards #-}
-- | A simple timer implementation running in @STM@ monad and using @async@ computations
-- to trigger timing.
module Control.Concurrent.Timer(
  -- * Type
  Timer,
  -- * Timer operations
  newTimer, start, reset, cancel,
  -- * Timer wait
  awaitSTM)
       where

import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM

data Timer = Timer { timer :: TVar (Maybe (Async.Async ()))  }

newTimer :: IO Timer
newTimer = Timer <$> newTVarIO Nothing

-- | Starts @timer@ to fire after given @i@ microseconds.
-- If @timer@ is already started, it is returned.
start :: Timer -> Int -> IO ()
start t i = do
  thread <- Async.async $ do
    threadDelay i
    return ()
  atomically $ writeTVar (timer t) (Just thread)

reset ::  Timer -> Int -> IO ()
reset t i = cancel t >> start t i

cancel :: Timer -> IO ()
cancel t = do
  tid <- atomically $ readTVar (timer t)
  maybe (return ()) Async.cancel tid

awaitSTM :: Timer -> STM ()
awaitSTM t = readTVar (timer t) >>= maybe (return ()) Async.waitSTM
