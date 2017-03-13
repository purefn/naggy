{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.OAuth2Provider where

import Data.Aeson.TH
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

data OAuth2Provider = OAuth2Provider
  { _oAuth2ProviderAuthorizationUrl :: URIRef Absolute
  , _oAuth2ProviderTokenUrl :: URIRef Absolute
  } deriving (Show, Eq)

$(deriveJSON (deriveJSONOptions 15) ''OAuth2Provider)

