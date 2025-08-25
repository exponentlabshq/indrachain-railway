{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module IndraCore.API 
  ( runAPIService
  ) where

import IndraCore.Config
import IndraCore.Types
import Servant
import Network.Wai.Handler.Warp
import System.IO (hPutStrLn, stderr)
import Data.Aeson

-- | API type definition
type IndraAPI = "health" :> Get '[JSON] HealthStatus
           :<|> "api" :> "status" :> Get '[JSON] NetworkStatus
           :<|> "metrics" :> Get '[PlainText] String

-- | Health status response
data HealthStatus = HealthStatus
  { status :: String
  , service :: String
  } deriving (Show, Generic)

instance ToJSON HealthStatus

-- | API server implementation
server :: Server IndraAPI
server = healthHandler :<|> statusHandler :<|> metricsHandler
  where
    healthHandler = return $ HealthStatus "healthy" "indracore-vm"
    
    statusHandler = return $ NetworkStatus
      { connectedPeers = 3
      , blockHeight = 1000
      , syncStatus = "synced"
      }
    
    metricsHandler = return "# IndraCore metrics\nindra_blocks_produced_total 1000\nindra_transactions_total 5000"

-- | Run the HTTP API service
runAPIService :: IndraConfig -> IO ()
runAPIService config = do
  hPutStrLn stderr $ "Starting API service on port: " ++ show (rpcPort config)
  
  let port = rpcPort config
  run port (serve (Proxy :: Proxy IndraAPI) server)