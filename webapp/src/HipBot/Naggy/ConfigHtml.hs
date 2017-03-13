{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.ConfigHtml
  ( configHtml
  ) where

import Data.Aeson (ToJSON(..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Time.Zones.All (toTZName)
import Lucid
import Protolude

configHtml :: Html ()
configHtml =
  doctypehtml_ $ do
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

encodeToText' :: ToJSON a => a -> Text
encodeToText' = toS . encodeToLazyText . toJSON

timezones :: [Text]
timezones = toS . toTZName <$> enumFromTo minBound maxBound

