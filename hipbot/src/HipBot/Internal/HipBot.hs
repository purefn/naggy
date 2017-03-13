{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HipBot.Internal.HipBot where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Wreq as Wreq
import Prelude

import HipBot.API
import HipBot.Descriptor
import HipBot.Types

type OnUninstall m = OAuthId -> m ()

data HipBot m = HipBot
  { _hipBotBotAPI :: HipBotAPI m
  , _hipBotAddOn :: AddOn
  , _hipBotOnUninstall :: OnUninstall m
  , _hipBotHTTPManager :: HTTP.Manager
  }

class HasHipBotAPI r m => HasHipBot r m | r -> m where
  hipBot :: Lens' r (HipBot m)

  hipBotAddOn :: Lens' r AddOn
  hipBotAddOn = hipBot . hipBotAddOn
  {-# INLINE hipBotAddOn #-}

  hipBotBotAPI :: Lens' r (HipBotAPI m)
  hipBotBotAPI = hipBot . hipBotBotAPI
  {-# INLINE hipBotBotAPI #-}

  hipBotHTTPManager :: Lens' r HTTP.Manager
  hipBotHTTPManager = hipBot . hipBotHTTPManager
  {-# INLINE hipBotHTTPManager #-}

  hipBotOnUninstall :: Lens' r (OnUninstall m)
  hipBotOnUninstall = hipBot . hipBotOnUninstall
  {-# INLINE hipBotOnUninstall #-}

instance HasHipBotAPI (HipBot m) m where
  hipBotAPI = hipBotBotAPI

instance HasHipBot (HipBot m) m where
  hipBot = id

  hipBotAddOn f (HipBot api addon uninstall manager) =
    fmap (\addon' -> HipBot api addon' uninstall manager) (f addon)
  {-# INLINE hipBotAddOn #-}

  hipBotBotAPI f (HipBot api addon uninstall manager) =
    fmap (\api' -> HipBot api' addon uninstall manager) (f api)
  {-# INLINE hipBotBotAPI #-}

  hipBotHTTPManager f (HipBot api addon uninstall manager) =
    fmap (HipBot api addon uninstall ) (f manager)
  {-# INLINE hipBotHTTPManager #-}

  hipBotOnUninstall f (HipBot api addon uninstall manager) =
    fmap (\uninstall' -> HipBot api addon uninstall' manager) (f uninstall)
  {-# INLINE hipBotOnUninstall #-}

newHipBot :: HasHipBotAPI r m => r -> AddOn -> OnUninstall m -> IO (HipBot m)
newHipBot api addon uninstall = HipBot (view hipBotAPI api) addon uninstall
  <$> HTTP.newManager tlsManagerSettings

newHipBot' :: Monad m => HipBotAPI m -> AddOn -> IO (HipBot m)
newHipBot' api addon = newHipBot api addon . const . pure $ ()

wreqDefaults :: HipBot m -> Wreq.Options
wreqDefaults bot = Wreq.defaults
  & Wreq.manager .~ Right (bot ^. hipBotHTTPManager)

onUninstall
  :: (MonadReader r m, HasHipBot r m)
  => OAuthId
  -> m ()
onUninstall oid = join . view $ (hipBotOnUninstall . to ($ oid))

