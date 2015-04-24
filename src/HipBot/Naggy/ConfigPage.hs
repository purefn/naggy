{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.ConfigPage
  ( configPage
  ) where

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import Data.Time.Zones.All
import Lucid
import Webcrank.Wai

import HipBot
import HipBot.Naggy.Session
import HipBot.Naggy.Types

configPage :: Registration -> NaggyCrank Body
configPage reg = writeSession reg $> body where
  body = lazyTextBody $ renderText $ doctypehtml_ $ do
    head_ $ do
      link_
        [ rel_ "stylesheet"
        , href_ "//www.hipchat.com/atlassian-connect/all.css"
        ]
      link_
        [ rel_ "stylesheet"
        , href_ "//aui-cdn.atlassian.com/aui-adg/5.8.11/css/aui.css"
        , media_ "all"
        ]
    body_ $ do
      with (script_ $ encodeToText' timezones) [ type_ "application/json", id_ "timezones" ]
      with (script_ "") [src_ "//www.hipchat.com/atlassian-connect/all.js"]
      with (script_ "") [src_ "//cdnjs.cloudflare.com/ajax/libs/jstimezonedetect/1.0.4/jstz.min.js"]
      with (script_ "") [src_ "/s/js/naggy.js"]

encodeToText' :: A.ToJSON a => a -> T.Text
encodeToText' = LT.toStrict . LT.toLazyText . A.encodeToTextBuilder . A.toJSON

timezones :: [T.Text]
timezones = T.decodeUtf8 . toTZName <$> enumFromTo minBound maxBound

-- Defined here for backwards compat with base <4.7
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
{-# INLINE ($>) #-}
