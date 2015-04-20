module HipBot.Naggy.API where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.List as List

import HipBot
import HipBot.Naggy.Types

insertReminder :: Reminder -> Naggy ()
insertReminder r = view naggyAPI >>= \api -> apiInsertReminder api r

lookupReminders
  :: OAuthId
  -> Naggy [Reminder]
lookupReminders oid = view naggyAPI >>= \api -> apiLookupReminders api oid

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
    , apiLookupReminders = (`stmLookupReminders` rs)
    , apiLookupReminder = \oid rid ->
        List.find (\r -> r ^. ident == rid) <$> stmLookupReminders oid rs
    , apiDeleteReminder = \oid rid ->
        liftIO . atomically . modifyTVar' rs $
          filter (\r -> r ^. oauthId /= oid && r ^. ident /= rid)
    }

stmLookupReminders :: (HasOauthId s a, MonadIO f, Functor f, Eq a) => a -> TVar [s] -> f [s]
stmLookupReminders oid = fmap (filter (\r -> r ^. oauthId == oid)) . liftIO . atomically . readTVar

