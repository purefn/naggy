{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Configurable where

import Data.Aeson.TH
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

data Configurable = Configurable
  { _configurableUrl :: URIRef Absolute
  } deriving (Show, Eq)

$(deriveJSON (deriveJSONOptions 13) ''Configurable)
