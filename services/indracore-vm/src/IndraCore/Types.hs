{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module IndraCore.Types where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text)
import Data.Map (Map)

-- | Node consensus role
data ConsensusRole 
  = Validator 
  | FullNode 
  | Archive 
  deriving (Show, Eq, Generic)

instance ToJSON ConsensusRole
instance FromJSON ConsensusRole

-- | Account address type
newtype Address = Address Text 
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Address
instance FromJSON Address

-- | Value/amount type
newtype Value = Value Integer 
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Value
instance FromJSON Value

-- | Transaction input reference
data TxOutRef = TxOutRef
  { txId :: Text
  , outputIndex :: Integer
  } deriving (Show, Eq, Generic)

instance ToJSON TxOutRef
instance FromJSON TxOutRef

-- | Transaction output
data TxOut = TxOut
  { value :: Value
  , datum :: Maybe Text
  , aaValidator :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TxOut
instance FromJSON TxOut

-- | Account information
data AccountInfo = AccountInfo
  { balance :: Value
  , nonce :: Integer
  , aaRules :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON AccountInfo
instance FromJSON AccountInfo

-- | Ledger state
data IndraLedger = IndraLedger
  { utxoSet :: Map TxOutRef TxOut
  , accountState :: Map Address AccountInfo
  } deriving (Show, Generic)

instance ToJSON IndraLedger
instance FromJSON IndraLedger

-- | Network status
data NetworkStatus = NetworkStatus
  { connectedPeers :: Integer
  , blockHeight :: Integer
  , syncStatus :: Text
  } deriving (Show, Generic)

instance ToJSON NetworkStatus
instance FromJSON NetworkStatus