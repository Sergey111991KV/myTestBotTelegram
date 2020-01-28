{-# LANGUAGE AllowAmbiguousTypes #-}

module Echo.EchoBot where

import Bot.Bot
import Control.Monad.State

class Bot b =>
      EchoBot b
  where
  helpMessage :: b -> String
  helpMessage _ = "Default echo-bot help message"
  repeatsCount :: b -> Int
  repeatsTxt :: b -> String
  isWaitingForRepeats :: b -> Bool
  setWaitingForRepeats :: Bool -> b -> b
  setRepeatsCount :: Int -> b -> b
  tryGetRepeatsCount :: BotMessage b -> Maybe Int

echoMessage :: EchoBot b => BotMessage b -> StateT b IO ()
echoMessage m = do
  let mId = messId m
  bot <- get
  if isWaitingForRepeats bot
    then maybe (return ()) (put . (`processRepeats` bot)) $ tryGetRepeatsCount m
    else case messText m of
           "/repeat" -> makeWaitingForRepeats mId
           "/help" ->
             sendMessageTo mId (helpMessage bot) >>
             modify (setWaitingForRepeats False)
           t ->
             replicateM_ (repeatsCount bot) (sendMessageTo mId t) >>
             modify (setWaitingForRepeats False)

processRepeats :: EchoBot b => Int -> b -> b
processRepeats r = setWaitingForRepeats False . setRepeatsCount r

makeWaitingForRepeats :: EchoBot b => Id (BotMessage b) -> StateT b IO ()
makeWaitingForRepeats mId = do
  bot <- get
  sendMessageTo mId $ repeatsTxt bot
  modify $ setWaitingForRepeats True

startEchoBot :: EchoBot b => BotConfig b -> IO ()
startEchoBot = evalStateT runEchoBot . getBotWithConfig

runEchoBot :: EchoBot b => StateT b IO ()
runEchoBot = do
  m <- getLastMessage
  maybe (return ()) echoMessage m
  runEchoBot
