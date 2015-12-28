{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


-- |A simple file-based event store.
module System.IO.Storage
       ( -- * Operations
         withStore,
         openStore,
         closeStore,
         -- * Types
         FileStore, Storage (..)) where

import           Prelude                   hiding (length, read)

import           Capital.Events.EventStore
import           Capital.Events.Source
import           Capital.Events.StoreOps
import           Capital.Log.Types
import           Capital.Utils.Concurrent
import           Control.Concurrent.STM    (TMVar, TQueue, TVar, atomically,
                                            modifyTVar', newEmptyTMVar,
                                            newTQueueIO, newTVar, putTMVar,
                                            readTQueue, readTVar, readTVarIO,
                                            takeTMVar, tryReadTMVar,
                                            tryTakeTMVar, writeTQueue)
import           Control.Exception         (SomeException, catch)
import           Control.Monad.Reader
import           Data.Binary.Builder
import           Data.Binary.Get
import           Data.ByteString.Lazy      (ByteString, hGet, hPut, length)
import           Data.Either               (isRight)
import qualified Data.Map                  as M
import           Data.Monoid               ((<>))
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           System.IO


-- |Internal definition of the storage to use for operations
data Storage = Storage  { storeName      :: String
                        , storeHandle    :: Maybe Handle
                          -- ^Handle to the underlying OS stream for storing events
                        , storeTid       :: TMVar Thread
                          -- ^The store's thread id, if store is open and running
                        , storeLog       :: Maybe Log
                          -- ^Logger for debugging and troubleshooting
                        , storeWaterMark :: TVar Mark
                        , storeTQueue    :: TQueue StoreOperation
                        }

instance HasLog Storage where
  getLog = storeLog

instance Task Storage where
  isAlive Storage{..} = atomically (tryReadTMVar storeTid) >>= maybe (return False) isAlive
  taskName = storeName
  killTask = void . closeStore
  waitInSTM Storage{..} = tryReadTMVar storeTid >>= maybe (return ()) waitInSTM

-- | A simple file store monad.
-- Handle to file is stored in a `ReaderT` wrapping an `IO`. `store` operations can be composed sequentially
newtype FileStore a = FileStore { runFileStore :: ReaderT Storage IO a }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Storage)

openStore :: Maybe Log -> FilePath -> IO Storage
openStore logger file = do
  tidvar  <- atomically newEmptyTMVar
  tq      <- newTQueueIO
  lookupvar <- atomically $ newTVar emptyMark
  h <- openFile file ReadWriteMode
  hSetBuffering h NoBuffering
  let s = Storage file (Just h) tidvar logger lookupvar tq
  tid <- spawn (runStore s)
  atomically $ putTMVar (storeTid s) tid
  maybeLog INFO ["store"] (StoreOpOpen $ T.pack file) logger
  return s

closeStore :: Storage -> IO Storage
closeStore s@(Storage _ h ltid l _ _) = do
  t <- liftIO $ atomically $ tryTakeTMVar ltid
  case t of
   Just tid -> liftIO $ kill tid
   Nothing  -> return ()
  void $ hClose `traverse` h
  maybeLog INFO ["store"] StoreOpClose l
  return s

runStore :: Storage -> IO ()
runStore s = forever $ do
  op <- atomically $ readTQueue (storeTQueue s)
  res <- runOp op (storeHandle s) (storeWaterMark s)
  maybeLog DEBUG ["store"] res (storeLog s)

duplicate ::  StorageParcel s -> Mark -> Bool
duplicate s m = case M.lookup (producerId s) m of
  Just sn -> sequenceNumber s /= (-1) && S.member (sequenceNumber s) sn
  Nothing -> False


runOp :: StoreOperation -> Maybe Handle -> TVar Mark -> IO StoreOpEvent
runOp _ Nothing _ = return $ StoreOpNoHandle
runOp (OpStore p r) (Just h) m =
  do
    v <- readTVarIO m
    let e = pdata p
    if (duplicate p v) then do
      atomically $ putTMVar r (Left "Duplicate event")
      return (StoreOpError "Duplicate Event")
      else
      do
        let s = doStore e
        opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (Right $ fromIntegral $ length s))
                 `catch` \ (ex  :: SomeException) -> return (Left $ "exception " ++ (show ex) ++ " while storing event " ++ (show e))
        when (isRight opres) $ atomically $ do
          mrk <- readTVar m
          let pid' = producerId p
          let s' = case M.lookup pid' mrk of
                   Just x -> S.insert (sequenceNumber p) x
                   Nothing -> S.insert (sequenceNumber p) S.empty
          modifyTVar' m $ M.insert pid' s'
        atomically $ putTMVar r opres
        return $ either (StoreOpError . T.pack) StoreOpWrite opres


runOp (OpLoad r) (Just h) _   =  do
  pos <- hTell h
  hSeek h SeekFromEnd 0
  sz <- hTell h
  hSeek h AbsoluteSeek 0
  opres <- readAll h sz
  hSeek h AbsoluteSeek pos
  atomically $ putTMVar r opres
  return $ StoreOpRead sz
    where
      readAll :: (Serializable s) => Handle -> Integer -> IO [s]
      readAll hdl sz =  if sz > 0 then
                          do
                            (e,ln) <- doLoad hdl
                            es     <- readAll hdl (sz - ln)
                            return $ e : es
                        else
                          return []
runOp (OpReset r) (Just handle) _ =
  do
    w <- (hIsWritable handle)
    case w of
     False -> atomically $ putTMVar r $ Left "File handle not writeable while resetting event store"
     True ->  do
       -- TODO catching all exceptions not a good idea, http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Exception.html#g:4
       -- but can't find out what hsetFileSize throws.
       v <- emptyEvents `catch` \ (ex  :: SomeException) -> return (Left $ "exception" ++ (show ex) ++ " while resetting event store")
       atomically $ putTMVar r v
       where emptyEvents = do
               (hSetFileSize handle 0)
               return $ Right ResetSuccess
    return StoreOpReset

runOp (OpGuard pid sn t) _ st = do
  mapping <- readTVarIO st
  let a = case M.lookup pid mapping of
              Just number -> S.member sn number
              Nothing -> False

  let guardResult = if a then Duplicate else New
  atomically $ putTMVar t guardResult
  return StoreOpGuard


-- | Run a `FileStore` operation against the provided `Storage`, producing some IO.
withStore :: Storage -> FileStore a -> IO a
withStore storage actions = runReaderT (runFileStore actions) storage

-- | Convert a serializable to ByteString for binary storage
doStore :: Serializable s => s -> ByteString
doStore e = let bs = write currentVersion e
                crc = 42  -- TODO compute real CRC32
            in toLazyByteString $
               (putWord32be (fromIntegral (length bs + 4 + 1)))
               <> singleton (fromIntegral currentVersion)
               <> putWord32be crc
               <> fromLazyByteString bs

-- |Read a single event from file store, returning also the number of bytes read
--
-- This is not symetric to doStore as we need first to read the length of the message, then
-- to read only the necessary amount of bytes from storage
doLoad :: Serializable s => Handle -> IO (s, Integer)
doLoad  h = do
  lw <- hGet h 4
  let l = fromIntegral $ runGet getWord32be lw
  bs <- hGet h l
  let msg = runGet $ do
        v   <- getWord8
        _   <- getWord32be
        pay <- getRemainingLazyByteString
        return $ read (fromIntegral v) pay
  return $ (msg bs, fromIntegral $ l + 4)


-- | A simple file-based store
-- We use same message format than Kafka (see http://kafka.apache.org/documentation.html#messages)
--
-- length : 4 bytes (does not include this field)
-- magic  : 1 byte
-- crc    : 4 bytes
-- payload: n bytes
instance EventStore FileStore where
  store          = eventAction' . OpStore
  load           = eventAction' OpLoad
  getMark pid sn = eventAction' $ (OpGuard pid sn)
  reset          = eventAction' OpReset

eventAction' ::  (TMVar b -> StoreOperation) -> FileStore b
eventAction' op = do
    s <- ask
    liftIO $ do
      tmv <- atomically $ do
        tmv <- newEmptyTMVar
        writeTQueue (storeTQueue s) (op tmv)
        return tmv
      res <- atomically $ takeTMVar tmv
      return res
