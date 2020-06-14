{-# LANGUAGE DeriveGeneric #-}

module Logging.Config
  ( module Logging.Internal.Logging
  , LogConfig(..)
  ) where

import Data.Aeson
import GHC.Generics
import Logging.Internal.Logging

data LogConfig = LogConfig
  { logFile :: FilePath
  , logLevel :: Logging
  , logConsole :: Bool
  } deriving (Show, Generic)

instance ToJSON LogConfig

instance FromJSON LogConfig
