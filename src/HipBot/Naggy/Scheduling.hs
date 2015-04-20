module HipBot.Naggy.Scheduling
  ( startReminder
  , stopReminder
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All
import Prelude hiding (elem)

import HipBot
import HipBot.Naggy.Types

startReminder :: Reminder -> Naggy ()
startReminder r = do
  tvar <- view threads
  let
    oid = r ^. oauthId
    rid = r ^. ident
  stopReminder oid rid
  tid <- forkNaggy (runReminder r)
  liftIO . atomically . modifyTVar' tvar . HashMap.insert (oid, rid) $ tid

runReminder :: Reminder -> Naggy ()
runReminder r = do
  -- TODO maybe log current time and how long we'll delay at debug level?
  liftIO . (threadDelay =<<) . microsToNext $ r
  bot <- view hipBot
  void $ sendNotification bot (r ^. oauthId) (r ^. roomId . to Right) (r ^. message . to TextNotification)
  -- TODO add logging of failures

microsToNext :: Reminder -> IO Int
microsToNext r = case r ^. repeating of
  Weekly _ days -> getCurrentTime <&> \nowUtc ->
    let
      z = tzByLabel $ r ^. tz
      tLocal = utcToLocalTimeTZ z nowUtc
      weekDay :: WeekDay
      weekDay = toEnum . (`mod` 7) . view _3 . toWeekDate . localDay $ tLocal
      tLocal' = tLocal { localTimeOfDay = TimeOfDay (r ^. time . hour) (r ^. time . minute) 0 }
    in
      diff nowUtc z $ if weekDay `elem` days && tLocal < tLocal'
        then tLocal'
        else addingDays tLocal' $ case Set.lookupGT weekDay days of
          Just d -> fromEnum d - fromEnum weekDay
          Nothing ->
            let d = Set.findMin days
            in fromEnum (maxBound :: WeekDay) - fromEnum weekDay + fromEnum d + 1

diff :: Integral c => UTCTime -> TZ -> LocalTime -> c
diff a z = round . (* 1000000) . flip diffUTCTime a . localTimeToUTCTZ z

addingDays :: Integral a => LocalTime -> a -> LocalTime
addingDays lt n = lt { localDay = addDays (fromIntegral n) (localDay lt) }

stopReminder :: OAuthId -> ReminderId -> Naggy ()
stopReminder oid rid  = do
  tvar <- view threads
  liftIO $ do
    tid <- atomically $ do
      ts <- readTVar tvar
      writeTVar tvar . HashMap.delete (oid, rid) $ ts
      return . HashMap.lookup (oid, rid) $ ts
    traverse_ killThread tid


