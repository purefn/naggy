{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.API
  ( HipBotAPI(..)
  , HasHipBotAPI(..)
  , apiInsertOAuthClient
  , apiDeleteOAuthClient
  , apiLookupOAuthClient
  , apiUpdateAccessToken
  ) where

import Control.Lens
import Control.Monad.Reader

import HipBot.Internal.URIRefOrphans ()
import HipBot.Types

data HipBotAPI m = HipBotAPI
  { _hipBotAPIInsertOAuthClient :: OAuthClient -> AccessToken -> m ()
  , _hipBotAPIDeleteOAuthClient :: OAuthId -> m ()
  , _hipBotAPILookupOAuthClient :: OAuthId -> m (Maybe (OAuthClient, AccessToken))
  , _hipBotAPIUpdateAccessToken :: OAuthId -> AccessToken -> m ()
  }

makeClassy ''HipBotAPI

apiInsertOAuthClient
  :: (MonadReader r m, HasHipBotAPI r m)
  => OAuthClient
  -> AccessToken
  -> m ()
apiInsertOAuthClient reg tok = view hipBotAPIInsertOAuthClient >>= \f -> f reg tok

apiDeleteOAuthClient
  :: (MonadReader r m, HasHipBotAPI r m)
  => OAuthId
  -> m ()
apiDeleteOAuthClient oid = view hipBotAPIDeleteOAuthClient >>= \f -> f oid

apiLookupOAuthClient
  :: (MonadReader r m, HasHipBotAPI r m)
  => OAuthId
  -> m (Maybe (OAuthClient, AccessToken))
apiLookupOAuthClient oid = view hipBotAPILookupOAuthClient >>= \f -> f oid

apiUpdateAccessToken
  :: (MonadReader r m, HasHipBotAPI r m)
  => OAuthId
  -> AccessToken
  -> m ()
apiUpdateAccessToken oid tok = view hipBotAPIUpdateAccessToken >>= \f -> f oid tok

