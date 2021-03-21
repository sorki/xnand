{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.Decide (
    Decide
  , decideParser
  , decideHandle
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Attoparsec.Text

import qualified System.Random

import           Plugins
import           Types

data Decide = DecideHelp
            | DecideCommand [Text]
            deriving (Show)

decideParser :: Parser Decide
decideParser =
      satisfy isEndOfLine *> pure DecideHelp
  <|> DecideCommand <$> (skipSpace *> (takeWhile1 (/='|') `sepBy` (skipSpace *> "|" <* skipSpace)))

decideHandle :: Decide -> PluginT App ()
decideHandle DecideHelp = reply "`#decide a | b | c` to use the best AI out there to decide for you"
decideHandle (DecideCommand xs) = do
  roll <- liftIO $ System.Random.randomRIO (0, length xs - 1)
  reply $ xs !! roll
