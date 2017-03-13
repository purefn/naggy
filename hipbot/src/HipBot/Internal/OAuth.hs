{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipBot.Internal.OAuth
  ( obtainAccessToken
  , OAuthError(..)
  , showOAuthError
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types
import Network.Wreq.Types (Postable)
import qualified Network.Wreq as Wreq
import Prelude
import URI.ByteString (URIRef, Absolute, serializeURIRef')

import HipBot.API
import HipBot.Descriptor
import HipBot.Internal.HipBot
import HipBot.Types

data OAuthError
  = MissingAccessTokenUrl OAuthClient
  | FetchCapabilitiesError OAuthClient HttpException
  | ParseCapabilitiesError OAuthClient Wreq.JSONError
  | FetchAccessTokenError OAuthClient (URIRef Absolute) HttpException
  | ParseAccessTokenError OAuthClient (URIRef Absolute) Wreq.JSONError
  | InvalidOAuthCreds OAuthClient

instance Show OAuthError where
  show = showOAuthError

showOAuthError :: OAuthError -> String
showOAuthError = \case
  MissingAccessTokenUrl r -> mconcat
    [ "Cannot get access token. Server capabilities at "
    , r ^. capabilitiesUrl . to uriStr
    , " is missing /capabilities/oauth2Provider/tokenUrl."
    ]
  FetchCapabilitiesError r err -> mconcat
    [ "Cannot get access token. Failure fetching HipChat server capabilities from "
    , r ^. capabilitiesUrl . to uriStr
    , ": "
    , show err
    ]
  ParseCapabilitiesError r err -> mconcat
    [ "Cannot get access token. Failure parsing HipChat server capabilities from "
    , r ^. capabilitiesUrl . to uriStr
    , ": "
    , show err
    ]
  FetchAccessTokenError _ turl err -> mconcat
    [ "Cannot get access token. Failure requesting access token from "
    , uriStr turl
    , ": "
    , show err
    ]
  ParseAccessTokenError _ turl err -> mconcat
    [ "Cannot get access token. Failure parsing access token response from "
    , uriStr turl
    , ": "
    , show err
    ]
  InvalidOAuthCreds _ -> mconcat
    [ "Cannot get access token. Authorization failed, indicating client is not longer valid. It has been removed."
    ]

obtainAccessToken
  :: (MonadReader r m, HasHipBot r m, MonadCatch m, MonadIO m)
  => OAuthClient
  -> m (Either OAuthError AccessToken)
obtainAccessToken client =
  runEitherT (getTokenUrl client >>= fetchAccessToken client)

getTokenUrl
  :: (MonadReader r m, HasHipBot r m, MonadIO m)
  => OAuthClient
  -> EitherT OAuthError m (URIRef Absolute)
getTokenUrl client =
  let
    tokenUrl' = preview (capabilities . _Just . oauth2Provider . _Just . tokenUrl)
    missing = left . MissingAccessTokenUrl $ client
    getUrl = maybe missing right . tokenUrl'
  in
    getUrl =<< fetchCapabilities client

fetchCapabilities
  :: (MonadReader r m, HasHipBot r m, MonadIO m)
  => OAuthClient
  -> EitherT OAuthError m AddOn
fetchCapabilities client =
  let
    fetch = flip getWith (client ^. capabilitiesUrl)
    asAddOn = fmap (^. Wreq.responseBody . to Right) . (Wreq.asJSON =<<)
    handleErr =
      handle (return . Left . FetchCapabilitiesError client) .
      handle (return . Left . ParseCapabilitiesError client)
  in
    EitherT . liftIO . handleErr . asAddOn . fetch =<< view (hipBot . to wreqDefaults)

fetchAccessToken
  :: (MonadReader r m, HasHipBot r m, MonadCatch m, MonadIO m)
  => OAuthClient
  -> URIRef Absolute
  -> EitherT OAuthError m AccessToken
fetchAccessToken client turl =
  let
    oid = client ^. oauthId . to T.encodeUtf8
    osecret = client ^. oauthSecret . to T.encodeUtf8
    opts bot = wreqDefaults bot & Wreq.auth ?~ Wreq.basicAuth oid osecret
    capScopes = view (hipBotAddOn . capabilities . folded . hipchatApiConsumer . folded . scopes)
    body bot = A.toJSON $ A.object
      [ "grant_type" .= A.String "client_credentials"
      , "scope" .= unwords (show <$> capScopes bot)
      ]
    fetch bot = do
      res <- liftIO $ do
        res <- postWith (opts bot) turl (body bot)
        Wreq.asJSON res
      if res ^. Wreq.responseStatus == unauthorized401
        then Left (InvalidOAuthCreds client) <$ apiDeleteOAuthClient (client ^. oauthId)
        else fmap Right . liftIO . resolveExpiresIn . view Wreq.responseBody $ res
    handleErr =
      handle (return . Left . FetchAccessTokenError client turl) .
      handle (return . Left . ParseAccessTokenError client turl)
  in
    EitherT . handleErr . fetch =<< view hipBot

uriStr :: URIRef a -> String
uriStr = B.toString . serializeURIRef'

getWith :: Wreq.Options -> URIRef Absolute -> IO (Wreq.Response LB.ByteString)
getWith opts = Wreq.getWith opts . uriStr

postWith
  :: Postable a
  => Wreq.Options
  -> URIRef Absolute
  -> a
  -> IO (Wreq.Response LB.ByteString)
postWith opts uri body = Wreq.postWith opts (uriStr uri) body

data HCAccessToken = HCAccessToken
  { _hcAccessToken:: Text
  , _hcExpiresIn :: Int
  }

instance A.FromJSON HCAccessToken where
  parseJSON = A.withObject "access token" $ \o -> HCAccessToken
    <$> o .: "access_token"
    <*> o .: "expires_in"

resolveExpiresIn :: HCAccessToken -> IO AccessToken
resolveExpiresIn t =
  let
    diff = realToFrac . _hcExpiresIn $ t
    expiring = addUTCTime diff
  in
    AccessToken (_hcAccessToken t) . expiring <$> getCurrentTime

