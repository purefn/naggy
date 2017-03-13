{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Naggy.Types where

import Control.Lens hiding ((.=))
import Control.Monad (fail)
import Control.Monad.Reader
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime
import Data.Time.Zones.All
import Protolude

import HipBot

type ReminderId = Text

data WeekDay
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance A.ToJSON WeekDay where
  toJSON d = case d of
    Sunday -> "Sunday"
    Monday -> "Monday"
    Tuesday -> "Tuesday"
    Wednesday -> "Wednesday"
    Thursday -> "Thursday"
    Friday -> "Friday"
    Saturday -> "Saturday"

instance A.FromJSON WeekDay where
  parseJSON = A.withText "weekday" $ \d -> case d of
    "Sunday" -> pure Sunday
    "Monday" -> pure Monday
    "Tuesday" -> pure Tuesday
    "Wednesday" -> pure Wednesday
    "Thursday" -> pure Thursday
    "Friday" -> pure Friday
    "Saturday" -> pure Saturday
    _ -> fail $ "invalid day of week '" <> T.unpack d <> "'"

data TimeWrapper = WrapTime { unWrapTime :: TimeOfDay }
  deriving Show

instance A.ToJSON TimeWrapper where
  toJSON (WrapTime (TimeOfDay h m _)) = A.object
    [ "hour" .= h
    , "minute" .= m
    ]

instance A.FromJSON TimeWrapper where
  parseJSON = A.withObject "object" $ fmap WrapTime . parseTimeOfDay where
    parseTimeOfDay o = TimeOfDay
      <$> (parseHours =<< o .: "hour")
      <*> (parseMinutes =<< o .: "minute")
      <*> pure 0

class HasHour s a | s -> a where
  hour :: Lens' s a

instance HasHour TimeOfDay Int where
  {-# INLINE hour #-}
  hour f (TimeOfDay h m s) = fmap (\h' -> TimeOfDay h' m s) (f h)

class HasMinute s a | s -> a where
  minute :: Lens' s a

instance HasMinute TimeOfDay Int where
  {-# INLINE minute #-}
  minute f (TimeOfDay h m s) = fmap (\m' -> TimeOfDay h m' s) (f m)

parseHours :: Int -> A.Parser Int
parseHours n = if n >= 0 && n < 24
  then return n
  else fail . mconcat $
    [ "expecting hour of day, but '"
    , show n
    , "' is out of range"
    ]

parseMinutes :: Int -> A.Parser Int
parseMinutes n = if n >= 0 && n < 60
  then return n
  else fail $ "expecting minutes of hour, but '" <> show n <> "' is out of range"

data Repeating
  = Weekly Int (Set WeekDay)
  -- | Daily Int
  -- | Monthly Int
  deriving Show

instance A.ToJSON Repeating where
  toJSON (Weekly n ds) = A.object
    [ "weekly" .= A.object
        [ "every" .= n
        , "days" .= ds
        ]
    ]

instance A.FromJSON Repeating where
  parseJSON = A.withObject "repeating" weekly where
    weekly o = o .: "weekly" >>= weekly'
    weekly' = A.withObject "weekly" $ \o -> Weekly
      <$> o .: "every"
      <*> o .: "days"

data Reminder = Reminder
  { _reminderIdent :: ReminderId
  , _reminderOauthId :: OAuthId
  , _reminderRoomId :: RoomId
  , _reminderInfo :: ReminderInfo
  } deriving Show

data ReminderInfo = ReminderInfo
  { _reminderInfoTime :: TimeOfDay
  , _reminderInfoTz :: TZLabel
  , _reminderInfoRepeating :: Repeating
  , _reminderInfoNotification :: NotificationMessage
  } deriving Show

makeFields ''ReminderInfo
makeFields ''Reminder

instance HasTime Reminder TimeOfDay where
  time = info .time

instance HasTz Reminder TZLabel where
  tz = info . tz

instance HasRepeating Reminder Repeating where
  repeating = info . repeating

instance HasNotification Reminder NotificationMessage where
  notification = info . notification

instance A.ToJSON Reminder where
  toJSON r = A.object
    [ "id" .= view ident r
    , "oauthId" .= view oauthId r
    , "roomId" .= view roomId r
    , "time" .= (WrapTime . view (info . time) $ r)
    , "tz" .= (T.decodeUtf8 . toTZName . view (info . tz) $ r)
    , "repeating" .= view (info . repeating) r
    , "notification" .= (WrapNotif . view (info . notification) $ r)
    ]

instance A.FromJSON Reminder where
  parseJSON = A.withObject "object" $ \o -> Reminder
    <$> o .: "id"
    <*> o .: "oauthId"
    <*> o .: "roomId"
    <*> A.parseJSON (A.Object o)

instance A.FromJSON ReminderInfo where
  parseJSON = A.withObject "object" $ \o -> ReminderInfo
    <$> fmap unWrapTime (o .: "time")
    <*> (parseTz =<< o .: "tz")
    <*> o .: "repeating"
    <*> fmap unWrapNotif (o .: "notification")

newtype NotifWrapper = WrapNotif { unWrapNotif :: NotificationMessage }

instance A.FromJSON NotifWrapper where
  parseJSON =
    let
      ntype o = o .: "type" >>= \case
        "text" -> return TextNotification
        "html" -> return HtmlNotification
        t -> fail $ "unrecognized message type '" <> T.unpack t <> "'"
      parse o = ntype o <*> o .: "message"
    in
      A.withObject "object" (fmap WrapNotif . parse)

instance A.ToJSON NotifWrapper where
  toJSON (WrapNotif n) = A.object $ case n of
    TextNotification t -> [ "type" .= ("text" :: Text), "message" .= t]
    HtmlNotification t -> [ "type" .= ("html" :: Text), "message" .= t]

parseTz :: Text -> A.Parser TZLabel
parseTz z = maybe badTz return . fromTZName . T.encodeUtf8 $ z where
  badTz = fail $ "unrecognized timezone '" <> T.unpack z <> "'"

