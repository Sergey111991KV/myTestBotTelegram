{-# LANGUAGE OverloadedStrings #-}

module Examples.Telegram.Internal.Requests
  -- ( sendTelegram
  -- ) 
  where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Examples.Requests.RequestBuilders
-- import Logging.Config
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP

-- sendTelegram :: LogConfig -> String -> [RequestParam] -> IO ()
-- sendTelegram LogConfig {logLevel = l, logFile = f} url params = do
--   let logManager =
--         if l == Debug
--           then tlsManagerSettings
--                  { managerModifyRequest =
--                      \r ->
--                        appendFile f "\nrequest to server: " >>
--                        appendFile f (show r) >>
--                        return r
--                  , managerModifyResponse =
--                      \r ->
--                        appendFile f "\nresponse status: " >>
--                        appendFile f (show $ responseStatus r) >>
--                        return r
--                  }
--           else tlsManagerSettings
--       body = buildRequestBody params
--   manager <- newManager logManager
--   request <- buildRequest url body
--   response <- httpLbs request manager
--   return ()

-- buildRequest :: String -> RequestBody -> IO Request
-- buildRequest url body = do
--   nakedRequest <- parseRequest url
--   return
--     (nakedRequest
--        { method = "POST"
--        , requestBody = body
--        , requestHeaders = [(HTTP.hContentType, "application/json")]
--        })
stt = (1,2)