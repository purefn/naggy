{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HipBot.Internal.URIRefOrphans where

import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.UTF8 (ByteString)
import qualified Data.Text.Encoding as T
import URI.ByteString (URIRef, Absolute, httpNormalization, laxURIParserOptions, normalizeURIRef', parseURI)

instance ToJSON (URIRef a) where
  toJSON = A.String . T.decodeUtf8 . normalizeURIRef' httpNormalization

instance FromJSON (URIRef Absolute) where
  parseJSON v = case v of
    A.String t -> parseAbsoluteURI . T.encodeUtf8 $ t
    x -> typeMismatch "URI" x

parseAbsoluteURI :: Monad m => ByteString -> m (URIRef Absolute)
parseAbsoluteURI s =
  let
    parsed = parseURI laxURIParserOptions s
    err e = fail . mconcat $
      [ "not an absolute URI ("
      , show e
      , ")"
      ]
  in
    either err pure parsed

