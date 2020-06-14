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

class Message (BotMessage b) => Bot b where -- новое наследование от Сообщения к боту
    type BotConfig b = c | c -> b -- благодаря расширению языка возможен такой синтаксис, происходит получение конфига
    type BotMessage b = m | m -> b
    getBotWithConfig :: BotConfig b -> b -- вытаскивание конфига
    getLastMtssage :: StateT b IO (Maybe (BotMessage b)) -- получение последнего сообщения если оно есть
    sendMessageTo :: Id (BotMessage b) -> String -> StateT b IO () -- получение Айди + сообщения -> преобразование этого всего в монаду State  с пустым сообщением

waitForMsg :: Bot b => StateT b IO (BotMessage b)
waitForMsg = do
  bot <- get
  msg <- getLastMessage
  case msg of
    (Just m) -> return m
    _ -> waitForMsg
        