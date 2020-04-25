{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Reader
import           Data.Text                      (Text)

import           Config
import           Frontend.Types

data Env = Env
  { config      :: Config
  , logQueue    :: TMQueue Text
  , frontend    :: Frontend
  }

type App = ReaderT Env IO
