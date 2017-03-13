{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.APIConsumer where

import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import HipBot.Internal.JSON

data APIConsumer = APIConsumer
  { _apiScopes :: [APIScope]
  , _apiFromName :: Maybe Text
  } deriving (Show, Eq)

defaultAPIConsumer :: APIConsumer
defaultAPIConsumer = APIConsumer [SendNotification] Nothing

data APIScope
  = AdminGroup
  | AdminRoom
  | ManageRooms
  | SendMessage
  | SendNotification
  | ViewGroup
  | ViewMessages
  deriving Eq

instance Show APIScope where
  show = apiScopeStr

apiScopeStr :: IsString a => APIScope -> a
apiScopeStr = \case
  AdminGroup -> "admin_group"
  AdminRoom -> "admin_room"
  ManageRooms -> "manage_rooms"
  SendMessage -> "send_message"
  SendNotification -> "send_notification"
  ViewGroup -> "view_group"
  ViewMessages -> "view_messages"

instance A.ToJSON APIScope where
  toJSON = A.String . apiScopeStr

instance A.FromJSON APIScope where
  parseJSON = A.withText "string" $ \case
    "admin_group" -> return AdminGroup
    "admin_room" -> return AdminRoom
    "manage_rooms" -> return ManageRooms
    "send_message" -> return SendMessage
    "send_notification" -> return SendNotification
    "view_group" -> return ViewGroup
    "view_messages" -> return ViewMessages
    s -> fail $ "unexpected API scope " <> T.unpack s

$(deriveJSON (deriveJSONOptions 4) ''APIConsumer)
