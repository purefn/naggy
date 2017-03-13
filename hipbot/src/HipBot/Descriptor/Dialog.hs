{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Dialog where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text (Text)
import URI.ByteString (URIRef, Absolute)

import HipBot.Descriptor.Name
import HipBot.Internal.URIRefOrphans ()
import HipBot.Internal.JSON

data Dialog = Dialog
  { _dialogKey :: Text
  , _dialogTitle :: Name
  , _dialogUrl :: URIRef Absolute
  , _dialogOptions :: Maybe DialogOptions
  } deriving (Show, Eq)

defaultDialog :: Text -> Name -> URIRef Absolute -> Dialog
defaultDialog k t u = Dialog k t u Nothing

data DialogStyle = Normal | Warning
  deriving (Show, Eq)

instance ToJSON DialogStyle where
  toJSON Normal = "normal"
  toJSON Warning = "warning"

instance FromJSON DialogStyle where
  parseJSON (String text) = case text of
    "normal" -> return Normal
    "warning" -> return Warning
    _ -> fail $ "Unexpected style string: " <> show text
  parseJSON x = typeMismatch "Invalid style" x

data DialogOptions = DialogOptions
  { _dialogOptionsStyle :: Maybe DialogStyle
  , _dialogOptionsPrimaryAction :: Maybe DialogAction
  , _dialogOptionsSecondaryActions :: Maybe [DialogAction]
  , _dialogOptionsSize :: Maybe DialogSize
  , _dialogOptionsHint :: Maybe Name
  , _dialogOptionsFilter :: Maybe DialogFilter
  } deriving (Show, Eq)

defaultDialogOptions :: DialogOptions
defaultDialogOptions = DialogOptions Nothing Nothing Nothing Nothing Nothing Nothing

data DialogAction = DialogAction
  { _dialogActionName    :: Name
  , _dialogActionEnabled :: Bool
  , _dialogActionKey     :: Maybe Text
  } deriving (Show, Eq)

data DialogSize = DialogSize
  { _dialogSizeHeight :: Text -- Either 'px' or '%'
  , _dialogSizeWidth  :: Text -- Either 'px' or '%'
  } deriving (Show, Eq)

data DialogFilter = DialogFilter
  { _dialogFilterPlaceholder :: Name
  } deriving (Show, Eq)

$(deriveJSON (deriveJSONOptions 7) ''Dialog)

$(deriveJSON (deriveJSONOptions 14) ''DialogOptions)

$(deriveJSON (deriveJSONOptions 13) ''DialogAction)

$(deriveJSON (deriveJSONOptions 11) ''DialogSize)

$(deriveJSON (deriveJSONOptions 13) ''DialogFilter)

