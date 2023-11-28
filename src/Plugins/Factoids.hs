{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Factoids (factoidsPlugin) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text
import Data.Attoparsec.Text
import System.FilePath

import Config
import Frontend.Types
import Plugins
import Types
import IRC

import Data.Factoid
import Data.Factoid.Schema

data Command = Get Text
             | Set Text Text
             | Forget Text
             | Help
             deriving (Show)

tok :: Parser Text
tok = takeWhile1 $ inClass $
     ['a' .. 'z']
  ++ ['A' .. 'Z']
  ++ ['0' .. '9']

parseFact :: Parser Command
parseFact =
      (pure Help <$> "help")
  <|> (Set <$> (tok <* skipSpace <* "=") <*> (skipSpace *> takeText))
  <|> (Forget <$> (tok <* "-forget"))
  <|> (Get <$> tok)

getDb :: Channel -> PluginT App Text
getDb c = do
  cs <- getChannelState c
  pure $ Data.Text.pack (cs </> "factoids.sqlite")

getFactoidsConfig :: PluginT App FactoidsConfig
getFactoidsConfig = do
  sender <- getSender
  pluginConfig <- lift $ asks (pluginConfigForSender sender . config)
  pure $ configFactoids pluginConfig

handleCommand :: Channel -> Command -> PluginT App ()
handleCommand c (Set k v) = do
  db <- getDb c
  by <- getUser
  liftIO $ setFactIO db k v by
  reply "Done"
handleCommand c (Get k) = do
  db <- getDb c
  mfact <- liftIO $ getFactIO db k
  case mfact of
    Just fact -> reply $ k <> " is " <> factoidValue fact
    Nothing -> reply $ "I don't know about " <> k
handleCommand c (Forget k) = do
  u <- getUser
  ops <- configOps <$> getFactoidsConfig

  if u `elem` ops then
    do
      db <- getDb c
      res <- liftIO $ forgetFactIO db k
      if res
          then reply "It's gone"
          else reply "404"
    else
      reply $ "I'm afraid I can't do that, " <> u
handleCommand _ Help = do
  prefixed <- configPrefixed <$> getFactoidsConfig
  reply $
    "?<factoid>"
    <> (if prefixed then mempty else " or <factoid>?")
    <> " to query, ?<factoid>=value to set, ?<factoid>-forget to unset"

factoidsPlugin :: Plugin
factoidsPlugin = Plugin
  { pluginName = "factoids"
  , pluginCatcher = \Input { inputMessage } ->
      case Data.Text.uncons inputMessage of
        Just ('?', command) -> Catched True $ parseOnly parseFact command
        _                   -> PassedOn
  , pluginHandler = \case
      Left err -> do
        liftIO $ print err
      Right command -> do
        mc <- getChannel
        maybe
          (pure ())
          (`handleCommand` command)
          mc
  }
