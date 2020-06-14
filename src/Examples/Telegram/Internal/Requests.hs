{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.Requests
  ( sendTelegram
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Examples.Requests.RequestBuilders
        