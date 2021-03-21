{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Frontend.Types
  ( Input(..)
  , inputUser
  , inputChannel
  , Output(..)
  , Frontend(..)
  , inputIRC
  , outputIRC
  ) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Data.Time.Clock                (UTCTime)
import           IRC
import qualified Network.AMQP                   as A

import Network.IRC.Bridge.Types

data Frontend = Frontend
  { outputQueue :: TMQueue Output
  , amqpChannel :: TMVar A.Channel
  }

data Input = Input
  { inputSender  :: Either User (Channel, User)
  , inputMessage :: Message
  } deriving (Show)

inputUser :: Input -> User
inputUser Input { inputSender = Left user }       = user
inputUser Input { inputSender = Right (_, user) } = user

inputChannel :: Input -> Maybe Channel
inputChannel Input { inputSender = Left _ }          = Nothing
inputChannel Input { inputSender = Right (chan, _) } = Just chan

data Output = Output
  { outputReceiver :: Either User Channel
  , outputMessage  :: Message
  } deriving Show

-- compat layer

outputIRC :: Output
          -> UTCTime
          -> IRCOutput
outputIRC Output{..} t = IRCOutput {
    outputTo = either IRCUser IRCChannel outputReceiver
  , outputBody = outputMessage
  , outputIsNotice = False
  , outputTime = t
  }

inputIRC :: IRCInput -> Input
inputIRC IRCInput{..} = Input {
    inputSender = cvt inputFrom inputSender
  , inputMessage = inputBody
  }
  where
    cvt (IRCUser user) _ = Left user
    cvt (IRCChannel channel) (Just user) = Right (channel, user)
    cvt (IRCChannel _) Nothing = error "Impossible .. channel message from no user"
