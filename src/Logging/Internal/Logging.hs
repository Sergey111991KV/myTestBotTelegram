{-# LANGUAGE DeriveGeneric #-}

module Logging.Internal.Logging where

import Data.Aeson
import GHC.Generics

data Logging
  = Debug
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON Logging

instance FromJSON Logging