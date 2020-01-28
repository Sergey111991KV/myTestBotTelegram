{-# LANGUAGE OverloadedStrings #-}

module Examples.Requests.RequestBuilders where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP

type RequestParam = (String, String)

buildRequestBody :: [RequestParam] -> RequestBody
buildRequestBody =
  RequestBodyBS . pack . ('{' :) . foldr buildBody "}" . fmap getParStr
  where
    getParStr (parName, parVal) =
      strToParam parName ++ ":" ++ strToParam parVal ++ ","
    strToParam s@('{':xs) = s
    strToParam s = "\"" ++ s ++ "\""
    buildBody p "}" = init p ++ "}"
    buildBody p r = p ++ r

buildQueryParams :: [RequestParam] -> String
buildQueryParams = reverse . foldl addParam ""
  where
    addParam [] par = getParStr par
    addParam pars par = pars ++ ('&' : getParStr par)
    getParStr (parName, parVal) = parName ++ "=" ++ parVal
