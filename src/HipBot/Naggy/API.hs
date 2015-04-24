{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.API where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as B
import qualified Data.List as List
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Zones.All
import Data.Typeable
import qualified Data.Vector as V
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Safe

import HipBot
import HipBot.Naggy.Types

insertReminder :: Reminder -> Naggy ()
insertReminder r = view naggyAPI >>= \api -> apiInsertReminder api r

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
deleteReminder oid rid = view naggyAPI >>= \api -> apiDeleteReminder api oid rid

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
    , apiDeleteReminder = \oid rid ->
        liftIO . atomically . modifyTVar' rs $
          filter (\r -> r ^. oauthId /= oid && r ^. ident /= rid)
    }

pgAPI :: Connection -> NaggyAPI
pgAPI conn = NaggyAPI
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
        liftIO . void . execute conn stmt $ row
  , apiFoldAllReminders = \f a ->
      let q = "select " <> pgFields <> " from naggy_reminders"
      in liftIO . PG.fold_ conn q a $ \b -> return . f b . unWrapRem
  , apiFoldReminders = \f a oid ->
      let q = "select " <> pgFields <> " from naggy_reminders where oauthId = ?"
      in liftIO . PG.fold conn q (Only oid) a $ \b -> return . f b . unWrapRem
  , apiLookupReminder = \oid rid ->
      let q = "select " <> pgFields <> " from naggy_reminders where oauthId = ? and id = ?"
      in liftIO . fmap (fmap unWrapRem . headMay) . query conn q $ (oid, rid)
  , apiDeleteReminder = \oid rid ->
      let stmt = "delete from naggy_reminders where oauthId = ? and id = ?"
      in liftIO . void . PG.execute conn stmt $ (oid, rid)
  }

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

