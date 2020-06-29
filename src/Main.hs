{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           IRC
import           Log
import           Plugins
import           Plugins.Commands
--import           Plugins.Commands.Tell
import           Types

import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Reader
import           Frontend.AMQP
import           Frontend.Types

main :: IO ()
main = do
  config' <- getConfig
  logQueue' <- newTMQueueIO
  frontend' <- initFrontend
  let env = Env config' logQueue' frontend'
  flip runReaderT env $ withLogging $
    runFrontend onInput

onInput :: Input -> App ()
onInput input = do
  ps <- plugins (inputSender input)
  _ <- runPlugins ps input
  return ()

examplePlugin :: Plugin
examplePlugin = Plugin
  { pluginName = "example"
  , pluginCatcher = Catched True
  , pluginHandler = \Input { inputSender } ->
      case inputSender of
        Left "srk" -> privMsg "srk" "I have received your message"
        Right ("bottest", user) -> chanMsg "bottest" $ user <> ": I have received your message"
        _ -> return ()
  }

developFilter :: Plugin
developFilter = Plugin
  { pluginName = "develop-filter"
  , pluginCatcher = \Input { inputSender } -> case inputSender of
      Left "srk" -> PassedOn

      Right ("bottest", _) -> PassedOn
      Right ("emci-spam", _) -> PassedOn

      _                    -> Catched True ()
  , pluginHandler = const (return ())
  }

plugins :: (MonadIO m, MonadReader Env m) => Either User (Channel, User) -> m [Plugin]
plugins sender = do
  pluginConfig <- asks (pluginConfigForSender sender . config)
  liftIO $ print pluginConfig

  debug <- asks (configDebugMode . config)

  let selectedPlugins =
        [ developFilter | debug ] ++
        [ commandsPlugin' | enableCommands pluginConfig ]

  liftIO $ putStrLn $ "For sender " ++ show sender ++ " using plugins " ++ show (map pluginName selectedPlugins)
  return selectedPlugins
