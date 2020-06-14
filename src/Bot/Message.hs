{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Message where

import Data.Text

class Show m => Message m where  -- наследование класса Месадж от стандартного Шоу 
    type Id m :: * -- однопараметрический тип
    messId :: m -> Id m -- получение  Id из данных, а все это получается благодаря расширениям
    messText :: m -> String -- тоже самое

