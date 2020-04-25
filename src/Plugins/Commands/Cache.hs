{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.Cache (
    Cache
  , cacheParser
  , cacheHandle
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BSC
import           Data.Attoparsec.Text

import           System.Nix.StorePath
import           Nix.NarInfo
import           ENix.Cached

import           Plugins
import           Types

data Cache = CacheHelp
           | CacheCommand Text
           deriving (Show)

cacheParser :: Parser Cache
cacheParser =
      satisfy isEndOfLine *> pure CacheHelp
  <|> CacheCommand <$> takeText

cacheHandle :: Cache -> PluginT App ()
cacheHandle CacheHelp = reply "Use `#cache(d) /nix/store/<path>` to query cache"
cacheHandle (CacheCommand txtPath) = do
  let bPath = BSC.pack $ T.unpack txtPath

  act <- case parsePath nixosStoreRoot bPath of
    Left e -> return $ T.pack $ "Path parse error - " ++ (unwords $ lines e)
    Right pth -> liftIO $ withTLS $ \man -> do
      res <- fetchNarInfo nixosCache man pth
      return $ case res of
        Right Nothing -> "Not cached"
        Right (Just NarInfo{..}) -> "Cached, size is " <> (T.pack $ prettySize fileSize)
        Left err -> T.pack err

  reply act
