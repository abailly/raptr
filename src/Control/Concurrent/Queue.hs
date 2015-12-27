-- | Provides a simple concurrent @Queue@ interface.
-- This queue is bounded so @put@ting to it while it is full is blocking.
module Control.Concurrent.Queue
       ( -- * Type
        Queue,
        -- * Creation
        empty, newQueueIO,
        -- * Operations
        put, take, flush)
       where

import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import           Prelude                        hiding (take)

newtype Queue a = Queue { queue :: Q.TBQueue a }

empty :: Int -> STM (Queue a)
empty = (Queue <$>) . Q.newTBQueue

newQueueIO :: Int -> IO (Queue a)
newQueueIO = (Queue <$>) . Q.newTBQueueIO

put :: Queue a -> a -> STM ()
put (Queue q) a = Q.writeTBQueue q a

take :: Queue a -> STM a
take (Queue q) = Q.readTBQueue q

flush :: Queue a -> STM [a]
flush (Queue q) = loop q []
  where
    loop :: Q.TBQueue a -> [ a ] -> STM [ a ]
    loop q out = Q.tryReadTBQueue q >>= maybe (return $ reverse out) (loop q . (:out))


