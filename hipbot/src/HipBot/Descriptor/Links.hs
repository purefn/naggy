{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Links where

import Data.Aeson.TH
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

data Links = Links
  { _linksSelf :: URIRef Absolute
  , _linksHomepage :: Maybe (URIRef Absolute)
  } deriving (Show, Eq)

defaultLinks
  :: URIRef Absolute  -- ^ self
  -> Links
defaultLinks s = Links s Nothing

$(deriveJSON (deriveJSONOptions 6) ''Links)
