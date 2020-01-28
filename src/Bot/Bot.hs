{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Bot
  ( Message(..)
  , Bot(..)
  , waitForMsg
  ) where

import Bot.Message
import Control.Monad.State
import Data.Maybe (isJust)

class Message (BotMessage b) =>
      Bot b
  where
  type BotConfig b = c| c -> b
  type BotMessage b = m| m -> b
  getBotWithConfig :: BotConfig b -> b
  getLastMessage :: StateT b IO (Maybe (BotMessage b))
  sendMessageTo :: Id (BotMessage b) -> String -> StateT b IO ()

waitForMsg :: Bot b => StateT b IO (BotMessage b)
waitForMsg = do
  bot <- get
  msg <- getLastMessage
  case msg of
    (Just m) -> return m
    _ -> waitForMsg
