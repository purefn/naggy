{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens hiding ((.=))
import Control.Monad (fail)
import Control.Monad.Reader
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime
import Data.Time.Zones.All (TZLabel, fromTZName, toTZName)
import Protolude hiding (to)

type ReminderId = Text

data NotificationMessage
  = TextNotification Text
  | HtmlNotification Text
  deriving (Show, Generic, NFData)

class HasMessage a b | a -> b where
  message :: Lens' a b

instance HasMessage NotificationMessage Text where
  message f = \case
    HtmlNotification t -> fmap HtmlNotification (f t)
    TextNotification t -> fmap TextNotification (f t)
  {-# INLINE message #-}

isHtmlNotification :: NotificationMessage -> Bool
isHtmlNotification = \case
  HtmlNotification _ -> True
  _ -> False

data WeekDay
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

showWeekDay :: IsString s => WeekDay -> s
showWeekDay = \case
  Sunday -> "Sunday"
  Monday -> "Monday"
  Tuesday -> "Tuesday"
  Wednesday -> "Wednesday"
  Thursday -> "Thursday"
  Friday -> "Friday"
  Saturday -> "Saturday"

instance A.ToJSON WeekDay where
  toJSON = showWeekDay

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

class HasHour s a | s -> a where
  hour :: Lens' s a

instance HasHour TimeOfDay Int where
  hour f (TimeOfDay h m s) = fmap (\h' -> TimeOfDay h' m s) (f h)
  {-# INLINE hour #-}

class HasMinute s a | s -> a where
  minute :: Lens' s a

instance HasMinute TimeOfDay Int where
  minute f (TimeOfDay h m s) = fmap (\m' -> TimeOfDay h m' s) (f m)
  {-# INLINE minute #-}

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
  deriving (Show, Generic, NFData)

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
  , _reminderInfo :: ReminderInfo
  } deriving (Show, Generic, NFData)

data ReminderInfo = ReminderInfo
  { _reminderInfoTimeOfDay :: TimeOfDay
  , _reminderInfoTz:: TZLabel
  , _reminderInfoRepeating :: Repeating
  , _reminderInfoNotification :: NotificationMessage
  } deriving (Show, Generic, NFData)

makeFields ''ReminderInfo
makeFields ''Reminder

instance HasMessage ReminderInfo Text where
  message = notification . message
  {-# INLINE message #-}

instance HasTimeOfDay Reminder TimeOfDay where
  timeOfDay = info . timeOfDay
  {-# INLINE timeOfDay #-}

instance HasRepeating Reminder Repeating where
  repeating = info . repeating
  {-# INLINE repeating #-}

instance HasNotification Reminder NotificationMessage where
  notification = info . notification
  {-# INLINE notification #-}

showt :: Show a => a -> Text
showt = show

instance A.ToJSON Reminder where
  toJSON r = A.object
    [ "id" .= view ident r
    , "timeOfDay" .= view (info . timeOfDay) r
    , "tz" .= view (info . tz . to WrapTZ) r
    , "repeating" .= view (info . repeating) r
    , "notification" .= (WrapNotif . view (info . notification) $ r)
    ]

instance A.FromJSON Reminder where
  parseJSON = A.withObject "object" $ \o -> Reminder
    <$> o .: "id"
    <*> A.parseJSON (A.Object o)

instance A.FromJSON ReminderInfo where
  parseJSON = A.withObject "object" $ \o -> ReminderInfo
    <$> o .: "timeOfDay"
    <*> fmap unwrapTZ (o .: "tz")
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

data TZWrapper = WrapTZ { unwrapTZ :: TZLabel }

instance A.ToJSON TZWrapper where
  toJSON = A.String . T.decodeUtf8 . toTZName . unwrapTZ

instance A.FromJSON TZWrapper where
  parseJSON = A.withText "string" (fmap WrapTZ . parseTz)

parseTz :: Text -> A.Parser TZLabel
parseTz z = maybe badTz return . fromTZName . T.encodeUtf8 $ z where
  badTz = fail $ "unrecognized timezone '" <> T.unpack z <> "'"


