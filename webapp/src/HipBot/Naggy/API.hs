{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Naggy.API where

import Control.Lens hiding ((&))
import Protolude hiding ((<>), to)

import HipBot
import HipBot.Naggy.Scheduling
import HipBot.Naggy.Types

data NaggyAPI m = NaggyAPI
  { _naggyAPISaveReminder :: Reminder -> m ()
  , _naggyAPIFoldAllReminders :: forall a. (a -> Reminder -> a) -> a -> m a
  , _naggyAPIFoldReminders :: forall a. (a -> Reminder -> a) -> a -> OAuthId -> m a
  , _naggyAPILookupReminder :: ReminderId -> m (Maybe Reminder)
  , _naggyAPIDeleteReminders :: OAuthId -> m ()
  , _naggyAPIDeleteReminder :: ReminderId -> m ()
  , _naggyAPIHipBotAPI :: HipBotAPI m
  }

makeClassy ''NaggyAPI

instance HasHipBotAPI (NaggyAPI m) m where
  hipBotAPI = naggyAPIHipBotAPI

saveReminder
  :: (MonadReader r m, HasNaggyAPI r m)
  => OAuthId
  -> RoomId
  -> ReminderInfo
  -> m Reminder
saveReminder oid room i = do
  rid <- registerReminder i
  let r = Reminder rid oid room i
  save <- view naggyAPISaveReminder
  r <$ save r

-- foldAllReminders :: (a -> Reminder -> a) -> a -> Naggy a
-- foldAllReminders f a = view naggyAPI >>= \api -> apiFoldAllReminders api f a

-- forAllReminders :: (Reminder -> Naggy ()) -> Naggy ()
-- forAllReminders f = join . foldAllReminders (\n -> (n *>) . f) $ return ()

foldReminders
  :: (MonadReader r m, HasNaggyAPI r m)
  => (a -> Reminder -> a)
  -> a
  -> OAuthId
  -> m a
foldReminders f a oid = view naggyAPIFoldReminders >>= \foldRs -> foldRs f a oid

lookupReminders
  :: (MonadReader r m, HasNaggyAPI r m)
  => OAuthId
  -> m [Reminder]
lookupReminders = foldReminders (flip (:)) []

lookupReminder
  :: (MonadReader r m, HasNaggyAPI r m)
  => ReminderId
  -> m (Maybe Reminder)
lookupReminder rid = view naggyAPILookupReminder >>= ($ rid)

deleteReminder
  :: (MonadReader r m, HasNaggyAPI r m)
  => ReminderId
  -> m ()
deleteReminder rid = cancelReminder rid >> view naggyAPIDeleteReminder >>= ($ rid)

deleteReminders
  :: (MonadReader r m, HasNaggyAPI r m)
  => OAuthId
  -> m ()
deleteReminders oid =
  cancelReminders oid >> view naggyAPIDeleteReminders >>= ($ oid)

