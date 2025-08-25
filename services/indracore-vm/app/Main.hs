module Main where

import IndraCore.VM
import IndraCore.Config
import CLI.Options
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

-- | Main entry point for IndraCore VM
main :: IO ()
main = do
  args <- getArgs
  config <- parseArgsToConfig args
  
  putStrLn "Starting IndraCore Virtual Machine..."
  putStrLn $ "Node ID: " ++ show (nodeId config)
  putStrLn $ "Environment: Railway Cloud"
  
  -- Initialize and start the VM
  runIndraCore config
  
-- | Parse command line arguments to configuration
parseArgsToConfig :: [String] -> IO IndraConfig
parseArgsToConfig _ = do
  -- Read configuration from environment variables
  -- This would be expanded with proper CLI parsing
  return defaultConfig
  
-- | Default configuration for Railway deployment
defaultConfig :: IndraConfig
defaultConfig = IndraConfig
  { nodeId = "indra-node-1"
  , consensusRole = Validator
  , networkPort = 9944
  , rpcPort = 9933
  , maxPeers = 50
  , railwayOptimized = True
  }