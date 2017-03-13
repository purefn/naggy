{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.API.PG
  ( MigrationCommand(..)
  , MigrationException(..)
  , executePool
  , pgAPI
  , queryPool
  , runMigrations
  , module HipBot.API
  ) where

import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.FileEmbed (embedFile)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only(..), Query, execute, query, withTransaction)
import Database.PostgreSQL.Simple.FromField (ResultError(..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.PostgreSQL.Simple.Migration (MigrationContext(..), MigrationCommand(..), MigrationResult(..), runMigration)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Safe (headMay)
import URI.ByteString (parseURI, strictURIParserOptions, serializeURIRef')

import HipBot.API
import HipBot.Internal.URIRefOrphans ()
import HipBot.Types

pgAPI :: (MonadIO m, MonadLogger m) => Pool Connection -> IO (HipBotAPI m)
pgAPI pool =
  let
    insert r t =
      let
        stmt = "insert into hipbot (" <> pgFields <> ") values (?, ?, ?, ?, ?, ?, ?)"
        row =
          ( r ^. oauthId
          , r ^. capabilitiesUrl . to serializeURIRef'
          , r ^. roomId
          , r ^. groupId
          , r ^. oauthSecret
          , t ^. accessToken
          , t ^. expires
          )
      in do
        $(logDebug) . mconcat $ [ "Inserting OAuth client ", r ^. oauthId ]
        liftIO . void . executePool pool stmt $ row

    delete oid = do
      $(logDebug) . mconcat $ [ "Deleting OAuth client ", oid ]
      let stmt = "delete from hipbot where oauthId = ?"
      liftIO . void . executePool pool stmt . Only $ oid

    lookupR =
      let q = "select " <> pgFields <> " from hipbot where oauthId = ?"
      in liftIO . fmap (fmap getRegRow . headMay) . queryPool pool q . Only

    update oid t =
      let
        stmt = "update hipbot set accessToken = ?, accessTokenExpires = ? where oauthId = ?"
        ps = (t ^. accessToken, t ^. expires, oid)
      in do
        $(logDebug) . mconcat $ [ "Updating access token for OAuth client ", oid ]
        liftIO . void . executePool pool stmt $ ps
  in do
    runMigrations pool migrations
    pure $ HipBotAPI insert delete lookupR update

data MigrationException = MigrationException String
  deriving Show

instance Exception MigrationException

runMigrations :: Pool Connection -> MigrationCommand -> IO ()
runMigrations pool ms = do
  migrated <- withResource pool $ \conn ->
    withTransaction conn . runMigration . MigrationContext ms False $ conn
  case migrated of
    MigrationError e -> throwIO $ MigrationException e
    MigrationSuccess -> pure ()

migrations :: MigrationCommand
migrations = mconcat
  [ MigrationInitialization
  , MigrationScript "hipbot.sql" $(embedFile "pg/hipbot.sql")
  ]

executePool :: ToRow q => Pool Connection -> Query -> q -> IO Int64
executePool pool stmt values =
  withResource pool $ \conn ->
    withTransaction conn (execute conn stmt values)

queryPool :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
queryPool pool q values =
  withResource pool $ \conn ->
    withTransaction conn (query conn q values)

pgFields :: Query
pgFields = "oauthId, capabilitiesUrl, roomId, groupId, oauthSecret, accessToken, accessTokenExpires"

newtype RegRow = RegRow { getRegRow :: (OAuthClient, AccessToken) }

instance FromRow RegRow where
  fromRow =
    let
      parseUri f =
        let
          err = returnError UnexpectedNull f ""
          err' e = returnError ConversionFailed f . mconcat $
            [ "not an absolute URI ("
            , show e
            , ")"
            ]
          parse = either err' pure . parseURI strictURIParserOptions

        in
          maybe err parse
      reg = OAuthClient
        <$> field
        <*> fieldWith parseUri
        <*> field
        <*> field
        <*> field
      tok = AccessToken <$> field <*> field
    in
      (RegRow .) . (,) <$> reg <*> tok

