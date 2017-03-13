{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Http
  ( HipBotHttpAPI
  , hipBotHttpAPI
  , hipBotServerT
  , module HipBot.Http.Auth
  ) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger, logError)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.Text as T
import Data.Proxy
import Servant

import HipBot.API
import HipBot.Descriptor
import HipBot.Http.Auth (JwtAuth, JwtAuthHandler, jwtAuthHandler)
import HipBot.Internal.HipBot
import HipBot.Internal.OAuth
import HipBot.Types

type HipBotHttpAPI
  = Get '[JSON] AddOn
  :<|> "installations" :> ReqBody '[JSON] OAuthClient :> PostNoContent '[JSON] NoContent
  :<|> "installations" :> Capture "oauthId" OAuthId :> JwtAuth :> DeleteNoContent '[JSON] NoContent

hipBotHttpAPI :: Proxy HipBotHttpAPI
hipBotHttpAPI = Proxy

hipBotServerT
  :: (MonadIO m, MonadReader r m, HasHipBot r m, MonadCatch m, MonadError ServantErr m, MonadLogger m)
  => ServerT HipBotHttpAPI m
hipBotServerT = getDescriptor
  :<|> registerClient
  :<|> unregisterClient

getDescriptor :: (MonadReader r m, HasHipBot r m) => m AddOn
getDescriptor = view hipBotAddOn

registerClient
  :: (MonadIO m, MonadReader r m, HasHipBot r m, MonadCatch m, MonadError ServantErr m, MonadLogger m)
  => OAuthClient
  -> m NoContent
registerClient client =
  let
    err e = do
      let msg = showOAuthError e
      $(logError) . mconcat $
        [ "Failed to register client '"
        , T.pack . show $ client & oauthSecret .~ "*****"
        , "': "
        , T.pack msg
        ]
      throwError (err502 { errBody = LB.fromString msg })
    success = (NoContent <$) . apiInsertOAuthClient client
  in
    either err success =<< obtainAccessToken client

unregisterClient
  :: (MonadIO m, MonadReader r m, HasHipBot r m, MonadError ServantErr m)
  => OAuthId
  -> OAuthClient
  -> m NoContent
unregisterClient oid client =
  if client ^. oauthId == oid
    then NoContent <$ onUninstall oid <* apiDeleteOAuthClient oid
    else throwError err403

