{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.API where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as B
import Data.Int
import qualified Data.List as List
import Data.Monoid
import Data.Pool
import qualified Data.Set as Set
import Data.Time.Zones.All
import Data.Typeable
import qualified Data.Vector as V
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Prelude
import Safe

import HipBot
import HipBot.Naggy.Scheduling
import HipBot.Naggy.Types

insertReminder :: Reminder -> Naggy ()
insertReminder r = do
  api <- view naggyAPI
  apiInsertReminder api r
  startReminder r

foldAllReminders :: (a -> Reminder -> a) -> a -> Naggy a
foldAllReminders f a = view naggyAPI >>= \api -> apiFoldAllReminders api f a

forAllReminders :: (Reminder -> Naggy ()) -> Naggy ()
forAllReminders f = join . foldAllReminders (\n -> (n *>) . f) $ return ()

foldReminders
  :: (a -> Reminder -> a)
  -> a
  -> OAuthId
  -> Naggy a
foldReminders f a oid = view naggyAPI >>= \api -> apiFoldReminders api f a oid

lookupReminders :: OAuthId -> Naggy [Reminder]
lookupReminders = foldReminders (flip (:)) []

lookupReminder
  :: OAuthId
  -> ReminderId
  -> Naggy (Maybe Reminder)
lookupReminder oid rid = view naggyAPI >>= \api -> apiLookupReminder api oid rid

deleteReminder
  :: OAuthId
  -> ReminderId
  -> Naggy ()
deleteReminder oid rid = do
  api <- view naggyAPI
  apiDeleteReminder api oid rid
  stopReminder oid rid

deleteReminders
  :: OAuthId
  -> Naggy ()
deleteReminders oid = do
  api <- view naggyAPI
  apiDeleteReminders api oid
  stopReminders oid

stmAPI :: IO NaggyAPI
stmAPI = do
  rs <- newTVarIO [] :: IO (TVar [Reminder])
  return NaggyAPI
    { apiInsertReminder = \r -> liftIO . atomically . modifyTVar' rs $ (r :)
    , apiFoldAllReminders = \f a ->
        liftIO . atomically . fmap (List.foldl' f a) . readTVar $ rs
    , apiFoldReminders = \f a oid ->
        liftIO .
          atomically .
          fmap (List.foldl' f a . filter (\r -> r ^. oauthId == oid)) .
          readTVar $
          rs
    , apiLookupReminder = \oid rid ->
        liftIO .
          atomically .
          fmap (List.find (\r -> r ^. oauthId == oid && r ^. ident == rid)) .
          readTVar $
          rs
    , apiDeleteReminders = \oid ->
        liftIO . atomically . modifyTVar' rs $
          filter (\r -> r ^. oauthId /= oid)
    , apiDeleteReminder = \oid rid ->
        liftIO . atomically . modifyTVar' rs $
          filter (\r -> r ^. oauthId /= oid && r ^. ident /= rid)
    }

pgAPI :: Pool Connection -> NaggyAPI
pgAPI pool = NaggyAPI
  { apiInsertReminder = \r ->
      let
        stmt = "insert into naggy_reminders (" <> pgFields <> ") values (?, ?, ?, ?, ?, ?, ?::weekday[], ?)"
        (every, ds) = case r ^. repeating of
          Weekly n days -> (n, fmap show . V.fromList . Set.toList $ days)
        row =
          ( r ^. ident
          , r ^. oauthId
          , r ^. roomId
          , r ^. time
          , r ^. tz .to toTZName
          , every
          , ds
          , r ^. message
          )
      in
        liftIO . void . executePool pool stmt $ row
  , apiFoldAllReminders = \f a ->
      let q = "select " <> pgFields <> " from naggy_reminders"
      in liftIO . foldPool_ pool q a $ \b -> return . f b . unWrapRem
  , apiFoldReminders = \f a oid ->
      let q = "select " <> pgFields <> " from naggy_reminders where oauthId = ?"
      in liftIO . foldPool pool q (Only oid) a $ \b -> return . f b . unWrapRem
  , apiLookupReminder = \oid rid ->
      let q = "select " <> pgFields <> " from naggy_reminders where oauthId = ? and id = ?"
      in liftIO . fmap (fmap unWrapRem . headMay) . queryPool pool q $ (oid, rid)
  , apiDeleteReminders = \oid ->
      let stmt = "delete from naggy_reminders where oauthId = ?"
      in liftIO . void . executePool pool stmt . Only $ oid
  , apiDeleteReminder = \oid rid ->
      let stmt = "delete from naggy_reminders where oauthId = ? and id = ?"
      in liftIO . void . executePool pool stmt $ (oid, rid)
  }

executePool :: ToRow q => Pool Connection -> Query -> q -> IO Int64
executePool pool stmt = withResource pool . (\row conn -> execute conn stmt row)

queryPool :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
queryPool pool q = withResource pool . (\a conn -> query conn q a)

foldPool_ :: FromRow r => Pool Connection -> Query -> b -> (b -> r -> IO b) -> IO b
foldPool_ pool q a = withResource pool . (\f conn -> PG.fold_ conn q a f)

foldPool :: (FromRow row, ToRow params) => Pool Connection -> Query -> params -> b -> (b -> row -> IO b) -> IO b
foldPool pool q ps a = withResource pool . (\f conn -> PG.fold conn q ps a f)

pgFields :: Query
pgFields = "id, oauthId, roomId, time, timezone, every, weekdays, message"

newtype RemWrapper = WrapRem { unWrapRem :: Reminder }

instance FromRow RemWrapper where
  fromRow = WrapRem <$> reminderRowParser

reminderRowParser :: RowParser Reminder
reminderRowParser = Reminder
  <$> field
  <*> field
  <*> field
  <*> field
  <*> fieldWith tzFieldParser
  <*> repeatingRowParser
  <*> field

tzFieldParser :: FieldParser TZLabel
tzFieldParser f = fmap unWrapTZLabel . maybe (unexpectedNull f) parse where
  parse = maybe err' (return . WrapTZLabel) . fromTZName where
    err' = returnError ConversionFailed f "unrecognized timezone label"

newtype TZLabelWrapper = WrapTZLabel { unWrapTZLabel :: TZLabel }
  deriving Typeable

repeatingRowParser :: RowParser Repeating
repeatingRowParser = Weekly
  <$> field
  <*> fmap (Set.fromList . V.toList . fmap unWrapWeekDay) field

newtype WeekDayWrapper = WrapWeekDay { unWrapWeekDay :: WeekDay }
  deriving Typeable

instance FromField WeekDayWrapper where
  fromField f = maybe (unexpectedNull f) parse where
    parse = maybe err' (return . WrapWeekDay) . readMay . B.toString
    err' = returnError ConversionFailed f "unrecognized week day"

unexpectedNull :: Typeable a => Field -> Conversion a
unexpectedNull f = returnError UnexpectedNull f ""

