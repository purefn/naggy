module Naggy.Ajax where

import Control.Functor
import Control.Monad.Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class
import Data.Either
import Data.JSON (FromJSON, encode, eitherDecode)
import Data.Maybe
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Naggy.Types

postReminder :: ReminderData -> Aff (ajax :: AJAX | _) Reminder
postReminder r = affjax req >>= decode where
  req = defaultRequest
    { method = POST
    , url = "/reminders"
    , headers = [ ContentType applicationJSON ]
    , content = Just $ encode r
    }

loadReminders :: Aff (ajax :: AJAX | _) [Reminder]
loadReminders = get "/reminders" >>= decode

deleteReminder :: Reminder -> Aff (ajax :: AJAX | _) Unit
deleteReminder (Reminder ident _) = delete_ ("/reminders/" ++ ident) $> unit

decode :: forall e a. (FromJSON a) => AffjaxResponse String -> Aff e a
decode r = either (throwError <<< error) pure $ eitherDecode r.response

