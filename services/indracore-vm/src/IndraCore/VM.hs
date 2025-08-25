{-# LANGUAGE OverloadedStrings #-}

module IndraCore.VM 
  ( runIndraCore
  , IndraConfig(..)
  , ConsensusRole(..)
  ) where

import IndraCore.Config
import IndraCore.Types
import IndraCore.Consensus
import IndraCore.Network
import IndraCore.API
import IndraCore.Monitoring
import Control.Concurrent.Async
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)

-- | Main IndraCore VM runner
runIndraCore :: IndraConfig -> IO ()
runIndraCore config = do
  hPutStrLn stderr "Initializing IndraCore VM components..."
  
  -- Start core services concurrently
  consensusAsync <- async (runConsensusService config)
  networkAsync <- async (runNetworkService config) 
  apiAsync <- async (runAPIService config)
  monitoringAsync <- async (runMonitoringService config)
  
  hPutStrLn stderr "IndraCore VM started successfully"
  hPutStrLn stderr $ "API available on port: " ++ show (rpcPort config)
  
  -- Keep main thread alive
  forever $ do
    threadDelay (60 * 1000000) -- 1 minute
    hPutStrLn stderr "IndraCore VM heartbeat..."
  
  -- Cleanup (this won't be reached in normal operation)
  mapM_ cancel [consensusAsync, networkAsync, apiAsync, monitoringAsync]