{-# LANGUAGE NoImplicitPrelude #-}

module HipBot.Naggy.Scheduling where

import Protolude

import HipBot
import HipBot.Naggy.Types

registerReminder :: ReminderInfo -> m ReminderId
registerReminder = notImplemented

cancelReminder :: ReminderId -> m ()
cancelReminder = notImplemented

cancelReminders :: OAuthId -> m ()
cancelReminders = notImplemented
