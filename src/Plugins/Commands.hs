{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands (commandsPlugin') where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Attoparsec.Text

import           Frontend.Types
import           Plugins
import           Plugins.Commands.Cache
import           Plugins.Commands.NARUrl
import           Types

data Command = Cached Cache
             | NARUrl NARUrl
             | Listing
             deriving (Show)

parseCommand :: Parser Command
parseCommand =
      ("cached" <|> "cache") *> skipSpace *> (Cached <$> cacheParser)
  <|> ("narurl" <|> "nar")   *> skipSpace *> (NARUrl <$> narUrlParser)
  <|> (pure Listing)

handleCommand :: Command -> PluginT App ()
handleCommand (Cached x)  = cacheHandle x
handleCommand (NARUrl x)  = narUrlHandle x
handleCommand Listing     = reply $ "Commands: " 
  <> T.unwords ["cache", "nar"]

commandsPlugin' :: Plugin
commandsPlugin' = Plugin
  { pluginName = "commands"
  , pluginCatcher = \Input { inputMessage } -> case T.uncons inputMessage of
      Just ('#', command) -> Catched True $ parseOnly parseCommand command
      _           -> PassedOn
  , pluginHandler = \case
      Left err -> do
        liftIO $ print err
        reply "Invalid command syntax"
      Right command -> handleCommand command
  }
