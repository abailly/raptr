-- | Core module for Raptr library.
--
-- Import this module to instantiate a @Raptr@ node member of a cluster communicating over HTTP.
module Network.Raptr.Raptr
       (module Network.Raptr.Server,
        module Network.Raptr.Types,
        module Network.Kontiki.Raft,
        module Network.Raptr.Client,
        -- * Types
        Raptr(..),
        -- * Configuration
        defaultConfig,
        -- * Control Server
        start,stop) where

import           Control.Concurrent.Async
import           Network.Kontiki.Raft
import           Network.Raptr.Client
import           Network.Raptr.Server
import           Network.Raptr.Types
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (cancel)
import           System.Random

data Raptr = Raptr { raptrPort   :: Port
                   , raptrThread :: Maybe (Async ())
                   }

defaultConfig = Raptr 0 Nothing

start :: Raptr -> Application -> IO Raptr
start config app = do
  let p = raptrPort config
  raptr <- if p == 0
    then do
    sock <- openSocket
    a <- async $ runSettingsSocket defaultSettings sock app
    port <- socketPort sock
    return $ config { raptrPort = fromIntegral port, raptrThread = Just a }
    else async (run p app) >>= \ tid -> return config { raptrThread = Just tid }
  putStrLn $ "starting raptr server on port " ++ show (raptrPort raptr)
  return raptr

stop :: Raptr -> IO ()
stop (Raptr _ Nothing)    = return ()
stop (Raptr p (Just tid)) =   putStrLn ("stopping raptr server on port " ++ show p) >> cancel tid

openSocket :: IO Socket
openSocket  = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromInteger 0) iNADDR_ANY)
  listen sock 5
  return sock


runRaptr :: IO Bool
runRaptr = return False


