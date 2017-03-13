{-# LANGUAGE OverloadedStrings #-}

module HipBot.Descriptor.Installable where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()

data Installable = Installable
  { _installableCallbackUrl :: Maybe (URIRef Absolute)
  , _installableAllowRoom :: Bool
  , _installableAllowGlobal :: Bool
  } deriving (Show, Eq)

defaultInstallable :: Installable
defaultInstallable = Installable Nothing True True

instance ToJSON Installable where
  toJSON (Installable cb r g) = object $ catMaybes
    [ ("callbackUrl" .=) <$> cb
    ] <>
    [ "allowRoom" .= r
    , "allowGlobal" .= g
    ]

instance FromJSON Installable where
  parseJSON = withObject "object" $ \o -> Installable
    <$> o .:? "callbackUrl"
    <*> o .:? "allowRoom" .!= True
    <*> o .:? "allowGlobal" .!= True

