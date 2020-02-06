-- | Provides a simple concurrent @Queue@ interface.
-- This queue is bounded but all operations are non-blocking so it is the
-- responsibility of the caller to handle success/failure of actions.
module Control.Concurrent.Queue
       ( -- * Type
        Queue,
        -- * Creation
        empty, newQueueIO,
        -- * Operations
        put, read, take, peek, flush)
       where

import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue as Q
import           GHC.Natural
import           Prelude                        hiding (read, take)

newtype Queue a = Queue { queue :: Q.TBQueue a }

empty :: Natural -> STM (Queue a)
empty = (Queue <$>) . Q.newTBQueue

newQueueIO :: Natural -> IO (Queue a)
newQueueIO = (Queue <$>) . Q.newTBQueueIO

-- | Try to put an element at 'end' of the queue
--
-- Returns `True` if operation succeeded, `False` otherwise.
put :: Queue a -> a -> STM Bool
put (Queue q) a = do
  full <- Q.isFullTBQueue q
  if not full
    then Q.writeTBQueue q a >> return True
    else return False

read :: Queue a -> STM a
read (Queue q) = Q.readTBQueue q

take :: Queue a -> STM (Maybe a)
take (Queue q) = Q.tryReadTBQueue q

peek :: Queue a -> STM (Maybe a)
peek (Queue q) = Q.tryPeekTBQueue q

flush :: Queue a -> STM [a]
flush (Queue q) = loop q []
  where
    loop :: Q.TBQueue a -> [ a ] -> STM [ a ]
    loop q out = Q.tryReadTBQueue q >>= maybe (return $ reverse out) (loop q . (:out))
