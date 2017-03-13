{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Naggy.API.PG
  ( pgAPI
  , module HipBot.Naggy.API
  ) where

import Control.Lens hiding ((&))
import Control.Monad.Logger (MonadLogger, logDebug)
import qualified Data.ByteString.UTF8 as B
import Data.FileEmbed (embedFile)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import qualified Data.Set as Set
import Data.Time.Zones.All (TZLabel, fromTZName, toTZName)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Protolude hiding ((<>), to)

import HipBot
import HipBot.API.PG (MigrationCommand(..), executePool, queryPool, runMigrations)
import qualified HipBot.API.PG as HipBot
import HipBot.Naggy.API
import HipBot.Naggy.Types

pgAPI :: (MonadLogger m, MonadIO m) => Pool Connection -> IO (NaggyAPI m)
pgAPI pool =
  let
    save r =
      let
        stmt = "insert into naggy_reminders (" <> pgFields <> ") values (?, ?, ?, ?, ?, ?, ?::weekday[], ?, ?)"
        (every, ds) = case r ^. repeating of
          Weekly n days -> (n, WrapDays days)
        (mtype, msg') = case r ^. notification of
          TextNotification t -> ("text" :: ByteString, t)
          HtmlNotification t -> ("html" :: ByteString, t)
        row =
          ( r ^. ident
          , r ^. oauthId
          , r ^. roomId
          , r ^. time
          , r ^. tz . to toTZName
          , every
          , ds
          , mtype
          , msg'
          )
      in do
        $(logDebug) . mconcat $ [ "Inserting reminder ", show r ]
        liftIO . void . executePool pool stmt $ row
    foldAll f a =
      let q = "select " <> pgFields <> " from naggy_reminders"
      in liftIO . foldPool_ pool q a $ \b -> return . f b . unWrapRem
    foldR f a oid =
      let q = "select " <> pgFields <> " from naggy_reminders where oauthId = ?"
      in liftIO . foldPool pool q (Only oid) a $ \b -> return . f b . unWrapRem
    lookupR =
      let q = "select " <> pgFields <> " from naggy_reminders where id = ?"
      in liftIO . fmap (fmap unWrapRem . headMay) . queryPool pool q . Only
    deleteRs oid =
      let stmt = "delete from naggy_reminders where oauthId = ?"
      in liftIO . void . executePool pool stmt . Only $ oid
    deleteR =
      let stmt = "delete from naggy_reminders where id = ?"
      in liftIO . void . executePool pool stmt . Only
   in do
     botAPI <- HipBot.pgAPI pool
     runMigrations pool migrations
     pure $ NaggyAPI save foldAll foldR lookupR deleteRs deleteR botAPI

migrations :: MigrationCommand
migrations = mconcat
  [ MigrationScript "naggy.sql" $(embedFile "pg/naggy.sql")
  ]

foldPool_ :: FromRow r => Pool Connection -> Query -> b -> (b -> r -> IO b) -> IO b
foldPool_ pool q a = withResource pool . (\f conn -> PG.fold_ conn q a f)

foldPool :: (FromRow row, ToRow params) => Pool Connection -> Query -> params -> b -> (b -> row -> IO b) -> IO b
foldPool pool q ps a = withResource pool . (\f conn -> PG.fold conn q ps a f)

pgFields :: Query
pgFields = "id, oauthId, roomId, time, timezone, every, weekdays, mtype, message"

newtype RemWrapper = WrapRem { unWrapRem :: Reminder }

instance FromRow RemWrapper where
  fromRow = WrapRem <$> reminderRowParser

reminderRowParser :: RowParser Reminder
reminderRowParser = Reminder
  <$> field
  <*> field
  <*> field
  <*> reminderInfoRowParser

reminderInfoRowParser :: RowParser ReminderInfo
reminderInfoRowParser = ReminderInfo
  <$> field
  <*> fieldWith tzFieldParser
  <*> repeatingRowParser
  <*> notificationRowParser

notificationRowParser :: RowParser NotificationMessage
notificationRowParser = fieldWith typeParser <*> field where
  typeParser f = maybe (unexpectedNull f) parse where
    parse = \case
      "text" -> return TextNotification
      "html" -> return HtmlNotification
      t -> returnError ConversionFailed f ("unrecognized notification type '" <> B.toString t <> "'")

tzFieldParser :: FieldParser TZLabel
tzFieldParser f = fmap unWrapTZLabel . maybe (unexpectedNull f) parse where
  parse = maybe err' (return . WrapTZLabel) . fromTZName where
    err' = returnError ConversionFailed f "unrecognized timezone label"

newtype TZLabelWrapper = WrapTZLabel { unWrapTZLabel :: TZLabel }

repeatingRowParser :: RowParser Repeating
repeatingRowParser = Weekly
  <$> field
  <*> fmap (Set.fromList . V.toList . fmap unWrapWeekDay) field

newtype DaysWrapper = WrapDays { unWrapDays :: Set WeekDay }

instance ToField DaysWrapper where
  toField = toField . fmap WrapWeekDay . V.fromList . toList . unWrapDays

newtype WeekDayWrapper = WrapWeekDay { unWrapWeekDay :: WeekDay }

instance ToField WeekDayWrapper where
  toField =
    let bs x = x :: ByteString
    in toField . bs . show . unWrapWeekDay

instance FromField WeekDayWrapper where
  fromField f = maybe (unexpectedNull f) parse where
    parse = maybe err' (return . WrapWeekDay) . readMaybe . B.toString
    err' = returnError ConversionFailed f "unrecognized week day"

unexpectedNull :: Typeable a => Field -> Conversion a
unexpectedNull f = returnError UnexpectedNull f ""

