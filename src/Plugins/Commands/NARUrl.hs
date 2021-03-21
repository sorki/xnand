{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.NARUrl (
    NARUrl
  , narUrlParser
  , narUrlHandle
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BSC
import           Data.Attoparsec.Text

import           System.Nix.StorePath
import           ENix.Cached

import           Plugins
import           Types

data NARUrl = NARUrlHelp
            | NARUrlCommand Text
            deriving (Show)

narUrlParser :: Parser NARUrl
narUrlParser =
      many space *> endOfInput *> pure NARUrlHelp
  <|> NARUrlCommand <$> takeText

narUrlHandle :: NARUrl -> PluginT App ()
narUrlHandle NARUrlHelp = reply "Use `#narUrl /nix/store/<path>` to query URL of the respective NAR"
narUrlHandle (NARUrlCommand txtPath) = do
  let bPath = BSC.pack $ T.unpack txtPath

  act <- case parsePath nixosStoreRoot bPath of
    Left e -> return $ T.pack $ "Path parse error - " ++ (unwords $ lines e)
    Right pth -> liftIO $ withTLS $ \man -> do
      res <- fetchNarInfo nixosCache man pth
      return $ case res of
        Right Nothing -> "Not cached"
        Right (Just narInfo) -> "URL: " <> (narURL nixosCache narInfo)
        Left err -> T.pack err

  reply act
