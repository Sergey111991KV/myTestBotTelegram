{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Message where

import Data.Text

class Show m =>
      Message m
  where
  type Id m :: *
  messId :: m -> Id m
  messText :: m -> String
