{-# LANGUAGE RecordWildCards #-}
-- | A simple timer implementation running in @STM@ monad and using @async@ computations
-- to trigger timing.
module Control.Concurrent.Timer(
  -- * Type
  Timer,
  -- * Timer operations
  start, reset, cancel,
  -- * Timer wait
  awaitSTM)
       where

import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM

-- NOTE This implementation isn't very safe

data Timer = Timer { timer :: Maybe (Async.Async ())  }

-- | Starts @timer@ to fire after given @i@ microseconds.
-- If @timer@ is already started, it is returned.
start :: Int -> IO Timer
start i = do
  thread <- Async.async $ do
    threadDelay i
    return ()
  return $ Timer (Just thread)

reset ::  Int -> Timer -> IO Timer
reset i t = cancel t >> start i

cancel :: Timer -> IO Timer
cancel (Timer (Just t)) = Async.cancel t >> return (Timer Nothing)
cancel t                = return t

awaitSTM :: Timer -> STM ()
awaitSTM (Timer (Just t)) = Async.waitSTM t
