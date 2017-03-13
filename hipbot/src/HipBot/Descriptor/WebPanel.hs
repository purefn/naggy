{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.WebPanel where

-- https://www.hipchat.com/docs/apiv2/webpanels

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics
import URI.ByteString (URIRef, Absolute)

import HipBot.Descriptor.Icon
import HipBot.Descriptor.Key
import HipBot.Descriptor.Name
import HipBot.Internal.JSON

data WebPanel = WebPanel
  { _webPanelIcon :: Maybe Icon
    -- ^ Icon to display on the left side of the webPanel title.

  , _webPanelKey :: Key
    -- ^ Unique key (in the context of the integration) to identify this
    -- webPanel. Valid length range: 1 - 40.

  , _webPanelLocation :: WebPanelLocation
    -- ^ The location of this webPanel Valid values: hipchat.sidebar.right.

  , _webPanelName :: Name
    -- ^ The display name of the webPanel.

  , _webPanelUrl :: URIRef Absolute
    -- ^ The URL of the resource providing the view content.

  , _webPanelWeight   :: Maybe Int
    -- ^ Determines the order in which webPanel appear. Web panels are
    -- displayed top to bottom or left to right in order of ascending weight. Defaults to 100.
  } deriving (Eq, Generic, Show)

data WebPanelLocation = HipchatSidebarRight
  deriving (Eq, Show)

instance ToJSON WebPanelLocation where
  toJSON HipchatSidebarRight = "hipchat.sidebar.right"

instance FromJSON WebPanelLocation where
  parseJSON (String "hipchat.sidebar.right") = return HipchatSidebarRight
  parseJSON (String x) = fail $ "Unexpected string: \"" <> T.unpack x <> "\" when parsing WebPanelLocation"
  parseJSON x = typeMismatch "WebPanelLocation" x

defaultWebPanel :: Key -> Name -> URIRef Absolute -> WebPanel
defaultWebPanel key' name url = WebPanel Nothing key' HipchatSidebarRight name url Nothing

$(deriveJSON (deriveJSONOptions 8) ''WebPanel)

