{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Glance where

-- https://www.hipchat.com/docs/apiv2/glances

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import URI.ByteString (URIRef, Absolute)

import HipBot.Descriptor.Icon
import HipBot.Descriptor.Key
import HipBot.Descriptor.Name
import HipBot.Internal.JSON

data GlanceTarget = GlanceKey Text -- ^ The key of a dialog, glance or web panel that should  be opened in response to this action. Valid length range: 1 - 40.
  deriving (Eq, Generic, Show)

instance ToJSON GlanceTarget where
  toJSON (GlanceKey x) = String x

instance FromJSON GlanceTarget where
  parseJSON (String x) = pure (GlanceKey x)
  parseJSON x = typeMismatch "GlanceTarget" x

data Glance = Glance
  { _glanceIcon :: Icon
    -- ^ Icon to display on the left side of the glance.

  , _glanceKey :: Key
    -- ^ Unique key (in the context of the integration) to identify this
    -- glance. Valid length range: 1 - 40.

  , _glanceName :: Name
    -- ^ The display name of the glance.

  , _glanceQueryUrl :: Maybe (URIRef Absolute)
    -- ^ The URL of the resource providing the glance content.

  , _glanceTarget :: Maybe GlanceTarget
    -- ^ Defines the behaviour when clicking on the glance.

  , _glanceWeight :: Maybe Integer
    -- ^ Determines the order in which glances appear. Glances are displayed
    -- top to bottom in order of ascending weight. Defaults to 100.
  } deriving (Eq, Generic, Show)

defaultGlance :: Icon -> Key -> Name -> Glance
defaultGlance i k n =
  Glance i k n Nothing Nothing Nothing

$(deriveJSON (deriveJSONOptions 7) ''Glance)

