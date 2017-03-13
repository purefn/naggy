{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.AddOn where

import Data.Aeson.TH
import Data.Text (Text)
import URI.ByteString (URIRef, Absolute)

import HipBot.Descriptor.Capabilities
import HipBot.Descriptor.Links
import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

data AddOn = AddOn
  { _addOnKey :: Text
  , _addOnName :: Text
  , _addOnDescription :: Text
  , _addOnLinks :: Links
  , _addOnCapabilities :: Maybe Capabilities
  , _addOnVendor :: Maybe Vendor
  } deriving (Show, Eq)

data Vendor = Vendor
  { _vendorUrl :: URIRef Absolute
  , _vendorName :: Text
  } deriving (Show, Eq)

defaultAddOn
  :: Text -- ^ key
  -> Text -- ^ name
  -> Text -- ^ description
  -> Links
  -> AddOn
defaultAddOn k n d ls = AddOn k n d ls Nothing Nothing

$(deriveJSON (deriveJSONOptions 7) ''Vendor)
$(deriveJSON (deriveJSONOptions 6) ''AddOn)
