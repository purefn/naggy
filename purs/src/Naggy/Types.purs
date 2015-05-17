module Naggy.Types where

import Data.Date
import Data.Date.Locale
import Data.Either
import Data.Foldable
import Data.JSON
import Data.Int (Int(), toNumber, fromNumber)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import Data.Time
import Data.Traversable
import Halogen
import Optic.Core

data Repeating
  = Weekly Number (Set.Set DayOfWeek)
  {-- | Daily Number --}
  {-- | Monthly Number --}

instance showRepeating :: Show Repeating where
  show (Weekly n days) = 
    let 
      e = if n == 1 then "Every week" else "Every " ++ show n ++ " weeks"
      ds = intercalate "," (show <$> Set.toList days)
    in
      e ++ " on " ++ ds

instance repeatingToJSON :: ToJSON Repeating where
  toJSON r = case r of
    Weekly n ds -> object 
      [ "weekly" .= object 
        [ "every" .= n
        , "days" .= ds
        ]
      ]

instance dayOfWeekToJSON :: ToJSON DayOfWeek where
  toJSON d = toJSON $ case d of
    Sunday -> "Sunday"
    Monday -> "Monday"
    Tuesday -> "Tuesday"
    Wednesday -> "Wednesday"
    Thursday -> "Thursday"
    Friday -> "Friday"
    Saturday -> "Saturday"

instance repeatingFromJSON :: FromJSON Repeating where
  parseJSON (JObject o) = (o .: "weekly") >>= \w -> case w of
    (JObject o) -> Weekly
      <$> (o .: "every")
      <*> (o .: "days")
    _ -> fail "expected object for Weekly"
  parseJSON _ = fail "expected object for Repeating"

instance dayOfWeekFromJSON :: FromJSON DayOfWeek where
  parseJSON (JString s) = case s of
    "Sunday" -> pure Sunday
    "Monday" -> pure Monday
    "Tuesday" -> pure Tuesday
    "Wednesday" -> pure Wednesday
    "Thursday" -> pure Thursday
    "Friday" -> pure Friday
    "Saturday" -> pure Saturday
    _ -> fail $ "invalid day " ++ s

newtype Time = Time 
  { hour :: HourOfDay
  , minute :: MinuteOfHour
  }

instance showTime :: Show Time where
  show (Time o) = show o.hour ++ ":" ++ show o.minute

instance showHourOfDay :: Show HourOfDay where
  show (HourOfDay i) = pad $ show $ toNumber i

instance showMinuteOfHour :: Show MinuteOfHour where
  show(MinuteOfHour i) = pad $ show $ toNumber i

pad s = if String.length s < 2 then "0" ++ s else s

instance timeToJSON :: ToJSON Time where
  toJSON (Time o) = object
    [ "hour" .= o.hour
    , "minute" .= o.minute
    ]

instance hourOfDayToJSON :: ToJSON HourOfDay where
  toJSON (HourOfDay i) = toJSON i

instance minuteOfHourToJSON :: ToJSON MinuteOfHour where
  toJSON (MinuteOfHour i) = toJSON i

instance intToJSON :: ToJSON Int where
  toJSON = toJSON <<< toNumber

instance timeFromJSON :: FromJSON Time where
  parseJSON (JObject o) = do
    h <- o .: "hour" 
    m <- o .: "minute"
    return $ Time { hour: h, minute: m }
  parseJSON _ = fail "expected object for Time"

instance hourOfDayFromJSON :: FromJSON HourOfDay where
  parseJSON v = parseJSON v >>= \n ->
    if n >= 0 && n < 24
      then pure $ HourOfDay $ fromNumber n
      else fail $ "not a valid hour of day '" ++ show n ++ "'"

instance minuteOfHourFromJSON :: FromJSON MinuteOfHour where
  parseJSON v = parseJSON v >>= \n ->
    if n >= 0 && n < 60
      then pure $ MinuteOfHour $ fromNumber n
      else fail $ "not a valid minute of hour '" ++ show n ++ "'"

mkTime :: HourOfDay -> MinuteOfHour -> Time
mkTime h m = Time { hour: h, minute: m }

unTime (Time o) = o

_Time :: LensP Time { hour :: HourOfDay, minute :: MinuteOfHour }
_Time f (Time o) = Time <$> f o

hour :: forall b a r. Lens { hour :: a | r } { hour :: b | r } a b
hour f o = f o.hour <#> \a -> o { hour = a }

minute :: forall b a r. Lens { minute :: a | r } { minute :: b | r } a b
minute f o = f o.minute <#> \a -> o { minute = a }

data Notification = TextNotification String | HtmlNotification String

isHtmlNotification :: Notification -> Boolean
isHtmlNotification n = case n of
  HtmlNotification _ -> true
  _ -> false

instance notificationToJSON :: ToJSON Notification where
  toJSON n = object $ case n of
    TextNotification t -> [ "type" .= "text", "message" .= t ]
    HtmlNotification t -> [ "type" .= "html", "message" .= t ]

instance notificationFromJSON :: FromJSON Notification where
  parseJSON (JObject o) = do
    f <- (o .: "type") >>= \t -> case t of
      "text" -> return TextNotification
      "html" -> return HtmlNotification
      _ -> fail ("unexpected type '" <> t <> "'")
    msg <- o .: "message"
    return (f msg)
  parseJSON _ = fail "expected object for Notification"

newtype ReminderData = ReminderData
  { time :: Time
  , tz :: String 
  , repeating :: Repeating
  , notification :: Notification
  }

instance reminderToJSON :: ToJSON ReminderData where
  toJSON (ReminderData o) = object
    [ "time" .= o.time
    , "tz" .= o.tz
    , "repeating" .= o.repeating
    , "notification" .= o.notification
    ]

instance reminderDataFromJSON :: FromJSON ReminderData where
  parseJSON (JObject o) = do
    t <- o .: "time"
    tz <- o .: "tz"
    r <- o .: "repeating"
    n <- o .: "notification"
    return $ ReminderData
      { time: t
      , tz: tz
      , repeating: r
      , notification: n
    }
  parseJSON _ = fail "expected object for ReminderData"

_ReminderData :: LensP ReminderData { time :: Time, tz :: String, repeating :: Repeating, notification :: Notification }
_ReminderData f (ReminderData o) = ReminderData <$> f o

time :: forall b a r. Lens { time :: a | r } { time :: b | r } a b
time f o = f o.time <#> \a -> o { time = a }

tz :: forall b a r. Lens { tz :: a | r } { tz :: b | r } a b
tz f o = f o.tz <#> \a -> o { tz = a }

repeating :: forall b a r. Lens { repeating :: a | r } { repeating :: b | r } a b
repeating f o = f o.repeating <#> \a -> o { repeating = a }

notification :: forall b a r. Lens { notification :: a | r } { notification :: b | r } a b
notification f o = f o.notification <#> \a -> o { notification = a }

message :: LensP Notification String
message = lens get set where
  get n = case n of
    TextNotification t -> t
    HtmlNotification t -> t
  set n t = case n of
    TextNotification _ -> TextNotification t
    HtmlNotification _ -> HtmlNotification t

data FormErrors = FormErrors (Maybe String) (Map.Map String String)

emptyFormErrors :: FormErrors
emptyFormErrors = FormErrors Nothing Map.empty

insertFieldError :: String -> String -> FormErrors -> FormErrors
insertFieldError n v (FormErrors se fe) = FormErrors se (Map.insert n v fe)

lookupFieldError :: String -> FormErrors -> Maybe String
lookupFieldError n (FormErrors _ fe) = Map.lookup n fe

deleteFieldError :: String -> FormErrors -> FormErrors
deleteFieldError n (FormErrors se fe) = FormErrors se (Map.delete n fe)

insertSubmissionError :: String -> FormErrors -> FormErrors
insertSubmissionError e (FormErrors _ fe) = FormErrors (Just e) fe

hasFieldErrors :: FormErrors -> Boolean
hasFieldErrors (FormErrors _ es) = not $ Map.isEmpty es

lookupSubmissionError :: FormErrors -> Maybe String
lookupSubmissionError (FormErrors se _) = se

deleteSubmissionError :: FormErrors -> FormErrors
deleteSubmissionError (FormErrors _ fe) = FormErrors Nothing fe

data ReminderForm = ReminderForm FormErrors ReminderData

data Reminder = Reminder String ReminderData

instance reminderFromJSON :: FromJSON Reminder where
  parseJSON v@(JObject o) = Reminder
    <$> (o .: "id")
    <*> (parseJSON v)

data Input
  = ShowForm ReminderForm
  | ShowList [Reminder]
  | AddReminder Reminder
  | DeleteReminder Reminder
  | CancelAdd

data Displaying = Loading | List | Form ReminderForm 

newtype State = State
  { displaying :: Displaying
  , reminders :: [Reminder]
  }

_State :: LensP State { displaying :: Displaying, reminders :: [Reminder] }
_State f (State o) = State <$> f o

displaying :: forall b a r. Lens { displaying :: a | r } { displaying :: b | r } a b
displaying f o = f o.displaying <#> \a -> o { displaying = a }

reminders :: forall b a r. Lens { reminders :: a | r } { reminders :: b | r } a b
reminders f o = f o.reminders <#> \a -> o { reminders = a }

initialState = State { displaying: Loading, reminders: [] }

type NaggyEffects eff = HalogenEffects 
  ( now :: Now
  , locale :: Locale
  | eff
  )

