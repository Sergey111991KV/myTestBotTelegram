{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.TelegramBot where

import Bot.Bot
import Control.Exception
import Control.Monad (void)
import Control.Monad.State
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (asum)
import Echo.EchoBot
import Examples.Exceptions.Exceptions
import Examples.Telegram.Internal.Requests
import Examples.Telegram.Internal.TelegramConfig
import Examples.Telegram.Internal.TelegramJson
import Logging.Config
import Network.HTTP.Conduit hiding (httpLbs)

data TelegramBot = TelegramBot 
    {congig :: TelegramConfig,  -- configuration
     botUrl :: String,
     getUpdates :: String,
     sendMessage :: String
  , lastMessId :: Integer
  , waitingForRepeats :: Bool
  }

instance Bot TelegramBot where -- заполнение типа БОТ
        type BotConfig TelegramBot = TelegramConfig -- use telegram config
        type BotMessage TelegramBot = TelegramMessage -- use telegrame message
        getBotWithConfig :: BotConfig b -> b -- вытаскивание конфига
        getLastMtssage :: StateT b IO (Maybe (BotMessage b)) -- получение последнего сообщения если оно есть
        sendMessageTo :: Id (BotMessage b) -> String -> StateT b IO () -- получение Айди + сообщения -> преобразование этого всего в монаду State  с пустым сообщением
    