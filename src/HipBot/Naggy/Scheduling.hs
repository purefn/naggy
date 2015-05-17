module HipBot.Naggy.Scheduling
  ( startReminder
  , stopReminder
  , stopReminders
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All
import Prelude hiding (elem)
import System.Log.Logger

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
  liftIO . atomically . modifyTVar' tvar $
    (at oid . non HashMap.empty . at rid .~ Just tid)

runReminder :: Reminder -> Naggy ()
runReminder r = do
  liftIO $ do
    delay <- microsToNext r
    now <- getCurrentTime
    debugM "naggy" $ mconcat
      [ "Pausing at "
      , show now
      , " for "
      , show delay
      , "ms for reminder "
      , show r
      ]
    threadDelay delay
  bot <- view hipBot
  e <- sendNotification bot (r ^. oauthId) (r ^. roomId . to Right) (r ^. notification)
  liftIO . traverse_ (errorM "naggy" . show) $ e
  runReminder r

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
      xs <- readTVar tvar
      writeTVar tvar $ xs & at oid . non HashMap.empty . at rid .~ Nothing
      return $ xs ^. at oid . non HashMap.empty . at rid
    traverse_ killThread tid

stopReminders :: OAuthId -> Naggy ()
stopReminders oid = do
  tvar <- view threads
  liftIO $ do
    tids <- atomically $ do
      xs <- readTVar tvar
      writeTVar tvar $ xs & at oid .~ Nothing
      return $ xs ^. at oid . non HashMap.empty . to HashMap.elems
    traverse_ killThread tids


