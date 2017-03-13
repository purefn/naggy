{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HipBot.Naggy.Descriptor where

import Control.Lens hiding ((&))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as BB
import URI.ByteString (URIRef, Absolute, pathL, urlEncodePath)
import URI.ByteString.QQ (uri)
import Protolude

import HipBot.Descriptor

(/++) :: URIRef a -> [ByteString] -> URIRef a
u /++ es =
  let
    append p = mconcat
      [ p
      , if B.last p /= '/' then "/" else ""
      , toS . BB.toLazyByteString . mconcat . intersperse "/" . fmap urlEncodePath $ es
      ]
  in
    over pathL append u

-- TODO can the links be made safer using safeLink from Servant?
descriptor :: URIRef Absolute -> AddOn
descriptor baseUri =
  defaultAddOn "com.atlassian.labs.naggy" "Naggy"
    "A bot for scheduling regular reminders (nags)."
    (defaultLinks baseUri
      & homepage ?~ baseUri)
    & capabilities ?~ (defaultCapabilities
      & installable ?~ (defaultInstallable
        & callbackUrl ?~ (baseUri /++ ["installations"])
        & allowRoom .~ True
        & allowGlobal .~ False)
      & hipchatApiConsumer ?~ (defaultAPIConsumer
        & scopes .~ [SendNotification]
        & fromName ?~ "Naggy")
      & configurable ?~ Configurable (baseUri /++ ["configure"]))
    & vendor ?~ Vendor [uri|https://atlassian.com/|] "Atlassian Labs"

