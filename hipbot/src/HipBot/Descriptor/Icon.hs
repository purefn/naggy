{-# LANGUAGE OverloadedStrings #-}

module HipBot.Descriptor.Icon where

-- Common icon objects

import Data.Aeson
import Data.Aeson.Types
import URI.ByteString (URIRef, Absolute)

import HipBot.Internal.URIRefOrphans ()

data Icon = Icon
  { _iconUrl   :: URIRef Absolute -- ^ Url for the icon.
  , _iconUrl2x :: URIRef Absolute -- ^ Url for the retina version of the icon.
  } deriving (Show, Eq)

instance ToJSON Icon where
  toJSON (Icon url url2x) = object
    [ "url"    .= url
    , "url@2x" .= url2x
    ]

instance FromJSON Icon where
  parseJSON (Object x) = Icon <$> x .: "url" <*> x .: "url@2x"
  parseJSON x = typeMismatch "Icon" x

newtype CompoundIcon = CompoundIcon
  { unCompoundIcon :: Either (URIRef Absolute) Icon
  } deriving Show

instance ToJSON CompoundIcon where
  toJSON (CompoundIcon (Left  x)) = toJSON x
  toJSON (CompoundIcon (Right x)) = toJSON x

