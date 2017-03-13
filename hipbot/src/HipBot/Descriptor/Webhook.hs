{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Webhook where

import Data.Aeson.TH
import Data.Text (Text)
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON
import HipBot.Types

data Webhook = Webhook
  { _webhookUrl :: URIRef Absolute
  , _webhookPattern :: Maybe Text
  , _webhookEvent :: RoomEvent
  } deriving (Show, Eq)

webhook :: URIRef Absolute -> RoomEvent -> Webhook
webhook url' = Webhook url' Nothing

$(deriveJSON (deriveJSONOptions 8) ''Webhook)
