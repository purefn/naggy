{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Naggy.Types where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Time.LocalTime
import Data.Time.Zones.All
import Web.ClientSession
import Webcrank.Wai

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

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
  else fail $ "expecting hour of day, but '" <> show n <> "' is out of range"

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
  , _reminderTime :: TimeOfDay
  , _reminderTz :: TZLabel
  , _reminderRepeating :: Repeating
  , _reminderMessage :: Text
  } deriving Show

makeFields ''Reminder

instance A.ToJSON Reminder where
  toJSON r = A.object
    [ "id" .= view ident r
    , "oauthId" .= view oauthId r
    , "roomId" .= view roomId r
    , "time" .= (WrapTime . view time $ r)
    , "tz" .= (T.decodeUtf8 . toTZName . view tz $ r)
    , "repeating" .= view repeating r
    , "message" .= view message r
    ]

instance A.FromJSON Reminder where
  parseJSON = A.withObject "object" $ \o -> Reminder
    <$> o .: "id"
    <*> o .: "oauthId"
    <*> o .: "roomId"
    <*> fmap unWrapTime (o .: "time")
    <*> (parseTz =<< o .: "tz")
    <*> o .: "repeating"
    <*> o .: "message"

parseTz :: Text -> A.Parser TZLabel
parseTz z = maybe badTz return . fromTZName . T.encodeUtf8 $ z where
  badTz = fail $ "unrecognized timezone '" <> T.unpack z <> "'"

data NaggyAPI = NaggyAPI
  { apiInsertReminder :: Reminder -> Naggy ()
  , apiFoldAllReminders :: forall a. (a -> Reminder -> a) -> a -> Naggy a
  , apiFoldReminders :: forall a. (a -> Reminder -> a) -> a -> OAuthId -> Naggy a
  , apiLookupReminder :: OAuthId -> ReminderId -> Naggy (Maybe Reminder)
  , apiDeleteReminders :: OAuthId -> Naggy ()
  , apiDeleteReminder :: OAuthId -> ReminderId -> Naggy ()
  }

data NaggyData = NaggyData
  { _naggyDataNaggyAPI :: NaggyAPI
  , _naggyDataHipBot :: HipBot Naggy
  , _naggyDataCsKey :: Key
  , _naggyDataThreads :: TVar (HashMap OAuthId (HashMap ReminderId ThreadId))
  }

initialNaggyData :: NaggyAPI -> HipBot Naggy -> IO NaggyData
initialNaggyData api bot = NaggyData api bot
  <$> fmap snd randomKey
  <*> newTVarIO HashMap.empty

type Session = (OAuthId, RoomId)

data NaggyReqState = NaggyReqState
  { _naggyReqStateSession :: Maybe Session
  , _naggyReqStateReminder :: Maybe Reminder
  }

initialNaggyReqState :: NaggyReqState
initialNaggyReqState = NaggyReqState Nothing Nothing

newtype Naggy a = Naggy { unNaggy :: ReaderT NaggyData (StateT NaggyReqState IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadReader NaggyData
    , MonadState NaggyReqState
    , MonadThrow
    )

naggy :: (NaggyData -> IO a) -> Naggy a
naggy = Naggy . ReaderT . fmap lift

runNaggy :: Naggy a -> NaggyData -> IO a
runNaggy n d = evalStateT (runReaderT (unNaggy n) d) initialNaggyReqState

forkNaggy :: Naggy () -> Naggy ThreadId
forkNaggy n = naggy (forkIO . runNaggy n)

type NaggyCrank = WaiCrankT Naggy

type NaggyResource = WaiResource Naggy

makeFields ''NaggyData
makeFields ''NaggyReqState

