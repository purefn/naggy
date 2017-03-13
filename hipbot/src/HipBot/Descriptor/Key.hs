{-# LANGUAGE OverloadedStrings #-}

module HipBot.Descriptor.Key where

-- Common length restricted key

import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text    (Text)
import qualified Data.Text as T

newtype Key = Key
  { unKey :: Text
  } deriving (Eq, Show)

_Key :: Prism' Text Key
_Key =
  let
    enc = unKey
    dec x =
      let
        len = T.length x
      in
        if len >= 1 && len <= 40
          then Just . Key $ x
          else Nothing
  in
    prism' enc dec

instance IsString Key where
  fromString x = fromJust (T.pack x ^? _Key)

instance ToJSON Key where
  toJSON (Key x) = toJSON x

instance FromJSON Key where
  parseJSON x = do
    rawKey <- parseJSON x
    case rawKey ^? _Key of
      Nothing -> fail $ "Invalid key length, expected [1,40], got: " <> show (T.length rawKey)
      Just k  -> return k

