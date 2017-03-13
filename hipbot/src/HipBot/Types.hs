{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Types where

import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Prelude
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

type OAuthId = Text
type RoomId = Int
type RoomName = Text

data RoomEvent
  = RoomMessage
  | RoomNotification
  | RoomExit
  | RoomEnter
  | RoomTopicChange
  deriving (Show, Eq)

instance A.ToJSON RoomEvent where
  toJSON s = A.String $ case s of
    RoomMessage -> "room_message"
    RoomNotification -> "room_notification"
    RoomExit -> "room_exit"
    RoomEnter -> "room_enter"
    RoomTopicChange -> "room_topic_change"

instance A.FromJSON RoomEvent where
  parseJSON = A.withText "string" $ \case
    "room_message" -> return RoomMessage
    "room_notification" -> return RoomNotification
    "room_exit" -> return RoomExit
    "room_enter" -> return RoomEnter
    "room_topic_change" -> return RoomTopicChange
    s -> fail $ "unexpected room event" <> T.unpack s

data OAuthClient = OAuthClient
  { _oAuthClientOauthId :: OAuthId
  , _oAuthClientCapabilitiesUrl :: URIRef Absolute
  , _oAuthClientRoomId :: Maybe RoomId
  , _oAuthClientGroupId :: Int
  , _oAuthClientOauthSecret :: Text
  } deriving (Show, Eq)

makeFields ''OAuthClient

$(deriveJSON (deriveJSONOptions 12) ''OAuthClient)

data AccessToken = AccessToken
  { _accessTokenAccessToken :: Text
  , _accessTokenExpires :: UTCTime
  } deriving (Show, Eq)

makeFields ''AccessToken

