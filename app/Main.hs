{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple


main :: IO ()
main = do
    response <- httpJSON "https://api.telegram.org/bot<823135486:AAHRRGLZFyKAa-hh7bkULT_fLJEn_2xorpg>/"
    
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)