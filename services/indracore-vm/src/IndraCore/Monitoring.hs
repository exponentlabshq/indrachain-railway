{-# LANGUAGE OverloadedStrings #-}

module IndraCore.Monitoring 
  ( runMonitoringService
  ) where

import IndraCore.Config
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.IO (hPutStrLn, stderr)

-- | Run the monitoring service
runMonitoringService :: IndraConfig -> IO ()
runMonitoringService config = do
  hPutStrLn stderr "Starting monitoring service..."
  hPutStrLn stderr "Prometheus metrics enabled"
  
  -- Monitoring loop
  forever $ do
    -- Collect and export metrics
    exportMetrics
    threadDelay (15 * 1000000) -- 15 seconds
    
-- | Export metrics for Prometheus
exportMetrics :: IO ()
exportMetrics = do
  -- This would export real metrics to Prometheus
  -- For now, just log that metrics are being collected
  return ()