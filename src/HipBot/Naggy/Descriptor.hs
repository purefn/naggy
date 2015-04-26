{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.Descriptor where

import Control.Lens
import HipBot

descriptor :: AbsoluteURI -> AddOn
descriptor baseUri =
  defaultAddOn "com.atlassian.labs.naggy" "Naggy"
    "A bot for scheduling regular reminders (nags)."
    (defaultLinks baseUri
      & homepage ?~ baseUri)
    & capabilities ?~ (defaultCapabilities
      & installable ?~ (defaultInstallable
        & callbackUrl ?~ baseUri `appendPath` ["installations"]
        & allowRoom .~ True
        & allowGlobal .~ False)
      & hipchatApiConsumer ?~ (defaultAPIConsumer
        & scopes .~ [SendNotification]
        & fromName ?~ "Naggy")
      & configurable ?~ Configurable (baseUri `appendPath` ["configure"]))
    & vendor ?~ Vendor "https://atlassian.com" "Atlassian Labs"

