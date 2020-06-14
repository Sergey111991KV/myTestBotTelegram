{-# LANGUAGE AllowAmbiguousTypes #-}

module Echo.EchoBot where

import Bot.Bot
import Control.Monad.State

class Bot b => EchoBot b where 
    where 
    helpMessage :: b -> String
    helpMessage _ = "Default Message" -- create default Message with any argument
    repeatsCount :: b -> Int
    repeatsText  :: b -> String
    isWaitingForRepeats  :: b -> Bool
    setWaitingForRepeats :: Bool -> b -> b
    setRepeatsCount :: Int -> b -> b
    tryGetRepeatsCount :: BotMessage b -> Maybe Int

echoMessage :: EchoBot b => BotMessage b -> StateT b IO ()
echoMessage m = do
    let mId = messId m
    bot <- get
    if isWaitingForRepeats bot 
        then maybe (return ()) 


    
    
    
    
    
    
    
    
    
    
    
    
        --   https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/sendMessage?chat_id=434218877&text=Hello