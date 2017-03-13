{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HipBot.Http.Auth where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Either (EitherT(..), eitherT, left, right)
import Crypto.Random (MonadRandom)
import Data.Bifunctor (first)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.Text.Encoding as T
import Jose.Jwt (JwtContent, JwtClaims(..))
import qualified Jose.Jwt as Jwt
import Jose.Jwk (Jwk(..))
import Network.Wai (Request, requestHeaders, queryString)
import Servant
import Servant.Server.Experimental.Auth

import HipBot.API
import HipBot.Internal.HipBot
import HipBot.Types

type JwtAuth = AuthProtect "hipchat-jwt-auth"

type instance AuthServerData JwtAuth = OAuthClient

type JwtAuthHandler = AuthHandler Request OAuthClient

data JwtAuthErr
  = MissingSignature
  | JwtError Jwt.JwtError
  | MissingClaims
  | UnknownClient
  deriving Show

jwtAuthHandler
  :: (MonadReader r m, HasHipBot r m, MonadError ServantErr m, MonadRandom m)
  => (m :~> Handler)
  -> AuthHandler Request OAuthClient
jwtAuthHandler nat =
  let
    jwtHeader = lookup "authorization" . requestHeaders
    jwtQuery = join . lookup "signed_request" . queryString
    jwt rq = maybe (left MissingSignature) right (jwtQuery rq <|> jwtHeader rq)
    auth = eitherT handleErr pure . (verify =<<) . jwt
  in
    mkAuthHandler (enter nat . auth)

verify
  :: (MonadReader r m, HasHipBotAPI r m, MonadRandom m)
  => ByteString
  -> EitherT JwtAuthErr m OAuthClient
verify jwt = do
  oid <- decodeOAuthId jwt
  client <- lookupClient oid
  client <$ decode client jwt

decodeOAuthId
  :: (Monad m)
  => ByteString
  -> EitherT JwtAuthErr m OAuthId
decodeOAuthId jwt = either
  (left . JwtError)
  (maybe (left MissingClaims) right . jwtIss . snd)
  (Jwt.decodeClaims jwt)

lookupClient
  :: (MonadReader r m, HasHipBotAPI r m)
  => OAuthId
  -> EitherT JwtAuthErr m OAuthClient
lookupClient = EitherT . fmap (maybe (Left UnknownClient) (Right . fst)) . apiLookupOAuthClient

decode
  :: (MonadRandom m)
  => OAuthClient
  -> ByteString
  -> EitherT JwtAuthErr m JwtContent
decode client =
  let jwk = SymmetricJwk (client ^. oauthSecret . to T.encodeUtf8) Nothing Nothing Nothing
  in EitherT . fmap (first JwtError) . Jwt.decode [ jwk ] Nothing

handleErr
  :: (MonadError ServantErr m)
  => JwtAuthErr
  -> m a
handleErr e =
  let
    errStatus = \case
      MissingSignature -> err401
      JwtError _ -> err401
      MissingClaims -> err401
      UnknownClient -> err403

    errMsg = \case
      MissingSignature -> "Request signature 'signed_request' not found"
      JwtError err -> mconcat
        [ "Request signature could not be decoded ('"
        , LB.fromString . show $ err
        , "')"
        ]
      MissingClaims -> "Request signature does not include claims"
      UnknownClient -> "Unknown installation"
  in
    throwError (errStatus e) { errBody = errMsg e }

