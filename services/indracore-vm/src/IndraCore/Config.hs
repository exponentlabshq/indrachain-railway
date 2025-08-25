{-# LANGUAGE OverloadedStrings #-}

module IndraCore.Config where

import IndraCore.Types
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | IndraCore configuration
data IndraConfig = IndraConfig
  { nodeId :: String
  , consensusRole :: ConsensusRole
  , networkPort :: Int
  , rpcPort :: Int
  , maxPeers :: Int
  , railwayOptimized :: Bool
  } deriving (Show)

-- | Load configuration from environment variables (Railway style)
loadConfigFromEnv :: IO IndraConfig
loadConfigFromEnv = do
  nodeId' <- fromMaybe "indra-node" <$> lookupEnv "NODE_ID"
  networkPort' <- readEnvInt "NETWORK_PORT" 9944
  rpcPort' <- readEnvInt "RPC_PORT" 9933
  maxPeers' <- readEnvInt "MAX_PEERS" 50
  role <- parseRole <$> lookupEnv "CONSENSUS_ROLE"
  
  return IndraConfig
    { nodeId = nodeId'
    , consensusRole = role
    , networkPort = networkPort'
    , rpcPort = rpcPort'
    , maxPeers = maxPeers'
    , railwayOptimized = True -- Always true on Railway
    }

-- | Read integer from environment with default
readEnvInt :: String -> Int -> IO Int
readEnvInt envVar defaultVal = do
  maybeVal <- lookupEnv envVar
  return $ case maybeVal >>= readMaybe of
    Just val -> val
    Nothing -> defaultVal

-- | Parse consensus role from string
parseRole :: Maybe String -> ConsensusRole
parseRole (Just "validator") = Validator
parseRole (Just "full-node") = FullNode  
parseRole (Just "archive") = Archive
parseRole _ = Validator -- Default to validator