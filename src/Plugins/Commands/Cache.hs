{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.Cache (
    Cache
  , cacheParser
  , cacheHandle
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BSC
import           Data.Attoparsec.Text
import qualified Data.Set

import           System.Nix.StorePath
import           Nix.NarInfo
import           ENix.Cached

import           Plugins
import           Types
import           Log

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
  lift $ logMsg $ "Handling cache"
  let bPath = BSC.pack $ T.unpack txtPath

  act <- case parsePath nixosStoreRoot bPath of
    Left e -> return $ T.pack $ "Path parse error - " ++ (unwords $ lines e)
    Right pth -> liftIO $ withTLS $ \man -> do
      res <- fetchNarInfo nixosCache man pth
      return $ case res of
        Right Nothing -> "Not cached"
        Right (Just NarInfo{..}) ->
             "Cached, size is "
          <> (T.pack $ prettySize fileSize)
          <> " decompressed from "
          <> compression
          <> " has "
          <> (T.pack $ prettySize narSize)
          <> " references "
          <> (case Data.Set.size references of
                0 -> "no paths"
                1 -> "one path"
                x -> (T.pack $ show x) <> " paths"
             )

        Left err -> T.pack err

  reply act
