{-# LANGUAGE OverloadedStrings #-}

module HipBot.Descriptor.Capabilities where

import Data.Aeson
import Data.Maybe (catMaybes, listToMaybe)

import HipBot.Descriptor.APIConsumer
import HipBot.Descriptor.Configurable
import HipBot.Descriptor.Dialog
import HipBot.Descriptor.Glance
import HipBot.Descriptor.Installable
import HipBot.Descriptor.OAuth2Provider
import HipBot.Descriptor.Webhook
import HipBot.Descriptor.WebPanel

data Capabilities = Capabilities
  { _capabilitiesInstallable :: Maybe Installable
  , _capabilitiesHipchatApiConsumer :: Maybe APIConsumer
  , _capabilitiesOauth2Provider :: Maybe OAuth2Provider
  , _capabilitiesWebhooks :: [Webhook]
  , _capabilitiesConfigurable :: Maybe Configurable
  , _capabilitiesDialog :: [Dialog]
  , _capabilitiesWebPanel :: [WebPanel]
  , _capabilitiesGlance :: [Glance]
  } deriving (Show, Eq)

defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities Nothing Nothing Nothing [] Nothing [] [] []

instance ToJSON Capabilities where
  toJSON (Capabilities is con o hs cfg dlg wp gl) = object $ catMaybes
    [ ("installable" .=) <$> is
    , ("hipchatApiConsumer" .=) <$> con
    , ("oauth2Provider" .=) <$> o
    , ("webhook" .= hs) <$ listToMaybe hs
    , ("configurable" .=) <$> cfg
    , ("dialog" .= dlg) <$ listToMaybe dlg -- TODO: port to Data.List.NonEmpty.nonEmpty
    , ("webpanel" .= wp) <$ listToMaybe wp
    , ("glance" .= gl) <$ listToMaybe gl
    ]

instance FromJSON Capabilities where
  parseJSON = withObject "object" $ \o -> Capabilities
    <$> o .:? "installable"
    <*> o .:? "hipchatApiConsumer"
    <*> o .:? "oauth2Provider"
    <*> o .:? "webhooks" .!= []
    <*> o .:? "configurable"
    <*> o .:? "dialog" .!= []
    <*> o .:? "webpanel" .!= []
    <*> o .:? "glance" .!= []

