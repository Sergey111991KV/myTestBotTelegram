-- import Network.HTTP.Client
-- import Network.HTTP.Types.Status (statusCode)

-- main :: IO ()
-- main = do
--   manager <- newManager defaultManagerSettings

--   request <- parseRequest "https://api.telegram.org/bot/823135486:AAHRRGLZFyKAa-hh7bkULT_fLJEn_2xorpg/getUpdates"
  

  
--   print request

-- {-# LANGUAGE OverloadedStrings #-}

-- import           Network.HTTP.Client      (newManager)
-- import           Network.HTTP.Client.TLS  (tlsManagerSettings)
-- import           Data.Text 

-- newtype Token = Token Text

-- runTelegramClient :: Token -> Manager -> TelegramClient a -> IO (Either ServantError a)

-- type TelegramClient a = ReaderT Token ClientM a

-- -- getWebhookInfoM

-- -- setWebhookRequest

-- -- setWebhookM

-- -- getMeM

-- main :: IO ()
-- main = do
--   let token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
--   manager <- newManager tlsManagerSettings
--   result <- runTelegramClient token manager $ do
--     info <- getWebhookInfoM
--     let request = setWebhookRequest' "https://example.com/hook"
--     isSet <- setWebhookM request
--     getMeM
--   print result
--   print "done!"

-- import           Data.Aeson.Parser           (json)
-- import           Data.Conduit                (($$))
-- import           Data.Conduit.Attoparsec     (sinkParser)
-- import           Network.HTTP.Client
-- import           Network.HTTP.Client.Conduit (bodyReaderSource)
-- import           Network.HTTP.Client.TLS     (tlsManagerSettings)
-- import           Network.HTTP.Types.Status   (statusCode)

-- main :: IO ()
-- main = do
--     manager <- newManager tlsManagerSettings

--     request <- parseRequest "https://api.telegram.org/bot1059314734:AAEFI9JK2VX9KWvBXKIrPn0VEdqCCy6YlFo/getUpdates"

--     withResponse request manager $ \response -> do
--         putStrLn $ "The status code was: " ++
--                    show (statusCode $ responseStatus response)

--         value <- bodyReaderSource (responseBody response)
--               $$ sinkParser json
--         print value


module Main where

import Control.Concurrent.Async
import Echo.EchoBot
-- import Examples.Slack.Bot
import Examples.Telegram.Bot

main = do
  telegramConfig <- getTelegramConfig
  -- slackConfig <- getSlackConfig
  async $ runTelegramEcho telegramConfig
  -- runSlackEcho slackConfig


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
  { config :: TelegramConfig
  , botUrl :: String
  , getUpdates :: String
  , sendMessage :: String
  , lastMessId :: Integer
  , waitingForRepeats :: Bool
  }

instance Bot TelegramBot where
  type BotConfig TelegramBot = TelegramConfig
  type BotMessage TelegramBot = TelegramMessage
  getBotWithConfig c =
    let bUrl = "https://api.telegram.org/bot" ++ token c ++ "/"
     in TelegramBot
          c
          bUrl
          (bUrl ++ "getUpdates")
          (bUrl ++ "sendMessage")
          0
          False
  getLastMessage = do
    tBot@TelegramBot {config = c, getUpdates = updStr, lastMessId = oldId} <-
      get
    let lc = logConfig c
        logL = logLevel lc
        logF = logFile lc
    upd <- liftIO $ catch (simpleHttp updStr) (handleGetException lc)
    let updates = eitherDecode upd :: Either String Updates
    let msg = processUpdates oldId updates
    put $ maybe tBot (\m -> tBot {lastMessId = message_id m}) msg
    return msg
  sendMessageTo mChat txt = do
    b@TelegramBot {config = c, sendMessage = sendUrl, waitingForRepeats = wr} <-
      get
    let lc = logConfig c
    liftIO $ sendText lc txt (chat_id mChat) sendUrl

sendText :: LogConfig -> String -> Integer -> String -> IO ()
sendText lc txt chatId sendUrl
  | txt == keyboard =
    catch
      (sendTelegram
         lc
         sendUrl
         [ ("chat_id", show chatId)
         , ("text", "please select repeats count:")
         , ("reply_markup", keyboard)
         ]) $
    handleSendException lc
  | otherwise =
    catch (sendTelegram lc sendUrl [("chat_id", show chatId), ("text", txt)]) $
    handleSendException lc

instance EchoBot TelegramBot where
  helpMessage = help . config
  repeatsCount = repeats . config
  repeatsTxt _ = keyboard
  isWaitingForRepeats = waitingForRepeats
  setWaitingForRepeats wFr b = b {waitingForRepeats = wFr}
  setRepeatsCount r b@TelegramBot {config = c} = b {config = c {repeats = r}}
  tryGetRepeatsCount m = lookup (messText m) keyboardAnswers
    where
      keyboardAnswers = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5)]

keyboard :: String
keyboard =
  "{\"keyboard\":[[\"1\",\"2\",\"3\",\"4\",\"5\"]],\"resize_keyboard\": true, \"one_time_keyboard\": true}"

processUpdates :: Integer -> Either String Updates -> Maybe TelegramMessage
processUpdates lastId = either (const Nothing) (findLastMessage lastId . result)

findLastMessage :: Integer -> [Update] -> Maybe TelegramMessage
findLastMessage oldId =
  asum .
  fmap
    (\u ->
       let mess = message u
        in if message_id mess > oldId
             then Just mess
             else Nothing)
