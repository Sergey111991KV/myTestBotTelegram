{-# LANGUAGE DeriveGeneric #-}

module Examples.Telegram.Internal.TelegramConfig where

import Data.Aeson
import GHC.Generics
import Logging.Config

data TelegramConfig = TelegramConfig
  { token :: String
  , help :: String
  , repeats :: Int
  , logConfig :: LogConfig
  } deriving (Show, Generic)

instance FromJSON TelegramConfig

instance ToJSON TelegramConfig
