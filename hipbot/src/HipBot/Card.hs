{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Card where

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON(..))
import Data.Aeson.TH (deriveToJSON)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (id)
import URI.ByteString (URIRef, Absolute)

import HipBot.Descriptor.Icon
import HipBot.Internal.JSON

--  file, image, application, link, media.
data CardStyle
  = CardFile
  | CardImage
  | CardApplication
  | CardLink
  | CardMedia
  deriving (Show, Eq)

instance ToJSON CardStyle where
  toJSON = \case
    CardFile -> "file"
    CardImage -> "image"
    CardApplication -> "application"
    CardLink -> "link"
    CardMedia -> "media"

data CardFormat
  = CardCompact
  | CardMedium
  deriving (Show, Eq)

instance ToJSON CardFormat where
  toJSON CardCompact = "compact"
  toJSON CardMedium = "medium"

data CardDescriptionFormat
  = CardDescriptionHtml
  | CardDescriptionText
  deriving (Show, Eq)

instance ToJSON CardDescriptionFormat where
  toJSON = \case
    CardDescriptionHtml -> "html"
    CardDescriptionText -> "text"

data CardDescription = CardDescription
  { _cardDescriptionFormat :: CardDescriptionFormat
  , _cardDescriptionValue :: Text
  } deriving (Show, Eq)

$(deriveToJSON (deriveJSONOptions 16) ''CardDescription)

data CardActivity = CardActivity
  { _cardActivityHtml :: Text
  , _cardActivityIcon :: Maybe Icon
  } deriving (Show, Eq)

$(deriveToJSON (deriveJSONOptions 13) ''CardActivity)

data CardAttributeValueStyle
  = LozengeSuccess
  | LozengeError
  | LozengeCurrent
  | LozengeComplete
  | LozengeMoved
  | Lozenge
  deriving (Show, Eq)

instance ToJSON CardAttributeValueStyle where
  toJSON = \case
    LozengeSuccess  -> "lozenge-success"
    LozengeError    -> "lozenge-error"
    LozengeCurrent  -> "lozenge-current"
    LozengeComplete -> "lozenge-complete"
    LozengeMoved    -> "lozenge-moved"
    Lozenge         -> "lozenge"

data CardAttributeValue = CardAttributeValue
  { _cardAttributeValueUrl   :: Maybe (URIRef Absolute)
  , _cardAttributeValueStyle :: Maybe CardAttributeValueStyle
  , _cardAttributeValueLabel :: Text
  , _cardAttributeValueIcon  :: Maybe Icon
  } deriving (Show, Eq)

$(deriveToJSON (deriveJSONOptions 19) ''CardAttributeValue)

data CardAttribute = CardAttribute
  { _cardAttributeLabel :: Text
  , _cardAttributeValue :: CardAttributeValue
  } deriving (Show, Eq)

$(deriveToJSON (deriveJSONOptions 14) ''CardAttribute)

data Card = Card
  { _cardStyle       :: CardStyle
  , _cardDescription :: CardDescription
  , _cardFormat      :: CardFormat
  , _cardUrl         :: Maybe (URIRef Absolute)
  , _cardTitle       :: Text
  , _cardActivity    :: Maybe CardActivity
  , _cardId          :: Text
  , _cardAttributes  :: [CardAttribute]
  } deriving (Show, Eq)

makeFields ''Card
$(deriveToJSON (deriveJSONOptions 5) ''Card)

cardWithDescription :: Text -> Text -> Card
cardWithDescription title' desc =
  let
    description' = CardDescription CardDescriptionHtml desc
    firstLine = fromMaybe "" (listToMaybe (T.lines desc))
  in
    Card
      { _cardStyle = CardApplication
      , _cardDescription = description'
      , _cardFormat = CardMedium
      , _cardUrl = Nothing
      , _cardTitle = title'
      , _cardActivity = Just $ CardActivity firstLine Nothing
      , _cardId = "12"
      , _cardAttributes = []
      }
