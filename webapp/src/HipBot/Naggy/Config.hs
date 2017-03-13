{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module HipBot.Naggy.Config
  ( Config
  , HasBaseUri(..)
  , HasPort(..)
  , HasDb(..)
  , readConfig
  ) where

import Control.Lens
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import Env (AsEmpty, AsUnread, AsUnset, Parser, Reader, auto, def, header, help, nonempty, parse, str, unread, var)
import Data.Monoid ((<>))
import Network (PortNumber)
import Protolude hiding ((<>))
import URI.ByteString (URIRef(URI), Absolute, Scheme(..), Authority(..), Host(..), laxURIParserOptions, parseURI)
import URI.ByteString.QQ (uri)

data Config = Config
  { _configBaseUri :: URIRef Absolute
  , _configPort :: PortNumber
  , _configDb :: ByteString
  } deriving (Show, Eq)

makeFields ''Config

readConfig :: IO Config
readConfig =
  let
    parser = Config
      <$> (baseUriFromMicrosDomain <|> baseUriEnv)
      <*> var auto "PORT" (def 8080 `mappend` help "port to run the http server on")
      <*> var bytestring "PG_NAGGY_URL" (def "" <> help "URI of the Postegres database to use for storage")
  in
    Env.parse (header "naggy") parser

baseUriEnv :: (AsUnset e, AsUnread e) => Parser e (URIRef Absolute)
baseUriEnv =
  var absUri "BASE_URL" (def [uri|http://localhost:8080/|] `mappend` help "naggy base URL")

absUri :: AsUnread e => Env.Reader e (URIRef Absolute)
absUri s = first (const . unread $ s) (parseURI laxURIParserOptions . toS $ s)

bytestring :: Env.Reader e ByteString
bytestring = Right . B.fromString

baseUriFromMicrosDomain :: (Env.AsEmpty e, AsUnset e) => Parser e (URIRef Absolute)
baseUriFromMicrosDomain = flip fmap microsDomain $ \d ->
  let
    scheme = Scheme "https"
    auth = Authority Nothing (Host d) Nothing
  in
    URI (scheme) (Just auth) "/" mempty Nothing

microsDomain :: (Env.AsEmpty e, AsUnset e) => Parser e ByteString
microsDomain = var (str <=< nonempty) "MICROS_SERVICE_DOMAIN_NAME" (help "domain name pointing to the ELB for naggy (e.g. naggy.internal.domain.dev.atlassian.io)")


