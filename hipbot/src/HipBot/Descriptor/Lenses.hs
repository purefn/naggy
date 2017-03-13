{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor.Lenses where

import Control.Lens

import HipBot.Descriptor.AddOn
import HipBot.Descriptor.APIConsumer
import HipBot.Descriptor.Capabilities
import HipBot.Descriptor.Configurable
import HipBot.Descriptor.Dialog
import HipBot.Descriptor.Glance
import HipBot.Descriptor.Icon
import HipBot.Descriptor.Installable
import HipBot.Descriptor.Links
import HipBot.Descriptor.Name
import HipBot.Descriptor.OAuth2Provider
import HipBot.Descriptor.Webhook
import HipBot.Descriptor.WebPanel

makeFields ''Dialog
makeFields ''DialogOptions
makeFields ''DialogAction
makeFields ''DialogSize
makeFields ''DialogFilter
makeFields ''Icon
makePrisms ''CompoundIcon
makeFields ''Glance
makeFields ''Name
makeFields ''WebPanel
makeFields ''AddOn
makeFields ''Vendor
makeFields ''Links
makeFields ''Capabilities
makeFields ''Installable
makeLensesWith abbreviatedFields ''APIConsumer
makeFields ''OAuth2Provider
makeFields ''Configurable
makeFields ''Webhook

