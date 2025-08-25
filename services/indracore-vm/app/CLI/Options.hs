module CLI.Options where

import IndraCore.Types
import IndraCore.Config

-- | Command line options parser
-- This is a minimal implementation for Railway deployment
data CLIOptions = CLIOptions
  { optNodeId :: String
  , optPort :: Int
  , optRPCPort :: Int
  , optRole :: ConsensusRole
  } deriving Show

-- | Parse CLI options (placeholder for Railway environment)
parseOptions :: [String] -> CLIOptions  
parseOptions _ = CLIOptions
  { optNodeId = "railway-node"
  , optPort = 9944
  , optRPCPort = 9933
  , optRole = Validator
  }