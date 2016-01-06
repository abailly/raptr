{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Core module for Raptr library.
--
-- Import this module to instantiate a @Raptr@ node member of a cluster communicating over HTTP.
module Network.Raptr.Raptr
       (module Network.Raptr.Server,
        module Network.Raptr.Types,
        module N,
        module Network.Kontiki.Raft,
        module Network.Raptr.Client,
        -- * Types
        Raptr(..),
        -- * Configuration
        defaultConfig, defaultRaftConfig, localCluster, FileLog(..), openLog,
        -- * Control Server
        startServer,start,stop) where

import           Control.Concurrent.Async
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Queue as Q
import           Control.Monad.Reader
import           Data.ByteString.Char8    (pack, unpack)
import qualified Data.Map                 as Map
import           Data.Maybe               (catMaybes, fromJust)
import           Data.Monoid              ((<>))
import qualified Data.Set                 as Set
import           Debug.Trace
import           Network.Kontiki.Raft
import           Network.Raptr.Client
import           Network.Raptr.Node       as N
import           Network.Raptr.Server
import           Network.Raptr.Types
import           Network.Socket
import           Network.URI              (nullURI, parseURI, parseURIReference,
                                           relativeTo, uriAuthority, uriPort)
import           Network.Wai
import           Network.Wai.Handler.Warp as W hiding (cancel)
import           System.FilePath          ((<.>))
import           System.IO.Storage
import           System.Random

data Raptr = Raptr { raptrPort   :: Port
                     -- ^Port this Raptr instance is listening on. May be set initially to 0 in which case
                     -- @start@ will allocate a new port
                   , raftConfig  :: Config
                     -- ^Raft cluster configuration, including this node's own id and other nodes ids
                   , raptrNodes  :: RaptrNodes
                     -- ^Map of nodes in the cluster, from @NodeId@ to @URI@
                   , raptrThread :: Maybe (Async ())
                     -- ^Thread for HTTP server
                   , nodeThread  :: Maybe (Async ())
                     -- ^Thread for Raft Node proper
                   }

instance Show Raptr where
  showsPrec p Raptr{..} = showParen (p >= 11) $
                          showString "Raptr { raptrPort = "
                          . showsPrec 11 raptrPort
                          . showString ", raftConfig = "
                          . showsPrec 11  raftConfig
                          . showString ", raptrNodes = "
                          . showsPrec 11  raptrNodes
                          . showString "}"

defaultRaftConfig :: Config
defaultRaftConfig = Config { _configNodeId = "unknown"
                           , _configNodes = Set.empty
                           , _configElectionTimeout = 4000 * 1000
                           , _configHeartbeatTimeout = 2000 * 1000
                           }

-- | Configures a local Raft cluster with given number of nodes
--
-- Nodes are run on 'localhost' with different ports whose range starts at 30700.
-- TODO: makes this more configurable, or remove
localCluster :: Int -> IO [ Raptr ]
localCluster numNodes = let nodeNames = take numNodes $ map (pack . ("node" <>) . show) [1 ..]
                            confs = map (\ nid -> defaultRaftConfig { _configNodeId = nid, _configNodes = Set.fromList nodeNames }) nodeNames
                            nodes = Map.fromList $ zip nodeNames (catMaybes $ map (parseURI . (\ p -> "http://localhost:" ++ p ++ "/raptr/") . show) [ 30700 .. ])
                            asURI bs = maybe nullURI id (parseURIReference $ unpack bs)
                        in mapM (\ c -> do
                                    t <- randomRIO (3000 * 1000, 7000 * 1000)
                                    return $ Raptr { raptrPort = (read . drop 1 . uriPort . fromJust) $ uriAuthority =<< Map.lookup (_configNodeId c) nodes
                                                   , raftConfig = c { _configElectionTimeout = t }
                                                   , raptrNodes = Map.map ( \ uri -> asURI (_configNodeId c) `relativeTo` uri) nodes
                                                   , raptrThread = Nothing , nodeThread = Nothing
                                                   }) confs

defaultConfig = Raptr 0 defaultRaftConfig emptyNodes Nothing Nothing

-- | Starts both server and Raft node for given configuration.
--
-- TODO: contains a log of hardwiring, needs to be improved by adding more configuration
-- to 'Raptr' type. Currently does the following:
--
--  * creates a new queue with maximum size 10
--  * creates a new Raft node with default configuraiton, storing data in a file named
--    according to 'nodeId'
--  * starts node asynchronously
--  * starts server
startServer :: Raptr -> IO Raptr
startServer r@Raptr{..} = do
  putStrLn $ "starting raptr " ++ show r
  q <- Q.newQueueIO 10
  m <- newMVar q
  let nodeid = _configNodeId raftConfig
  log <- openLog $ unpack nodeid <.> "log"
  node <- newNode (Just q) raftConfig  (Client raptrNodes) log
  nodethread <- async $ runReaderT (runServer (N.run raftConfig initialState)) node
  start (r { nodeThread = Just nodethread }) (server node)


start :: Raptr -> Application -> IO Raptr
start r@Raptr{..} app = do
  let p = raptrPort
  raptr <- if p == 0
           then startOnRandomPort
           else async (W.run p app) >>= \ tid -> return r { raptrThread = Just tid }
  putStrLn $ "starting raptr server " ++ show raptr
  return raptr
    where
      startOnRandomPort = do
        sock <- openSocket
        a <- async $ W.runSettingsSocket defaultSettings sock app
        port <- socketPort sock
        return r { raptrPort = fromIntegral port, raptrThread = Just a }


stop :: Raptr -> IO ()
stop (raptrThread -> Nothing)      = return ()
stop r@(raptrThread -> (Just tid)) = putStrLn ("stopping raptr server on port " ++ show (raptrPort r)) >> cancel tid

openSocket :: IO Socket
openSocket  = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromInteger 0) iNADDR_ANY)
  listen sock 5
  return sock

runRaptr :: IO Bool
runRaptr = return False


