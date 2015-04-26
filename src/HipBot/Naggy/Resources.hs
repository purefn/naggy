{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.Resources
  ( remindersResource
  , reminderResource
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai as Wai
import Webcrank.Wai

import HipBot
import HipBot.Naggy.API as Naggy
import HipBot.Naggy.Session
import HipBot.Naggy.Types

remindersResource :: NaggyResource
remindersResource = resource
  { allowedMethods = return [ methodGet, methodPost ]
  , isAuthorized = lift checkAuthorization
  , postAction = liftIO UUID.nextRandom <&>
      PostCreate . pure . T.decodeUtf8 . UUID.toASCIIBytes
  , contentTypesAccepted = return [("application/json", putReminder)]
  , contentTypesProvided = return [("application/json", getReminders)]
  }

parseReminder :: ReminderId -> OAuthId -> RoomId -> LB.ByteString -> HaltT NaggyCrank Reminder
parseReminder rid oid room b =
  let
    parser = A.parseJSON . A.Object .
      HashMap.insert "id" (A.toJSON rid) .
      HashMap.insert "oauthId" (A.toJSON oid) .
      HashMap.insert "roomId" (A.toJSON room)
    r = do
      v <- A.eitherDecode b
      A.parseEither (A.withObject "object" parser) v
    failure err = do
      writeLBS . LB.fromString $ err
      halt badRequest400
  in
    either failure return r

putReminder :: HaltT NaggyCrank ()
putReminder = withSession $ \(oid, room) -> do
  [rid] <- getDispatchPath
  r <- parseReminder rid oid room =<< liftIO . Wai.lazyRequestBody =<< view request
  lift . lift $ do
    insertReminder r
    reminder ?= r
  putResponseHeader hContentType "application/json"
  writeLBS . A.encode $ r

getReminders :: HaltT NaggyCrank Body
getReminders = withSession $ \(oid, _) -> do
  rs <- lift . lift $ lookupReminders oid
  return . lazyTextBody . LT.toLazyText . A.encodeToTextBuilder . A.toJSON $ rs

-- TODO allow PUT
reminderResource :: T.Text -> NaggyResource
reminderResource rid = resource
  { allowedMethods = return [ methodDelete ]
  , isAuthorized = lift checkAuthorization >>= \a -> case a of
      Authorized -> withSession $ \(oid, _) -> lift . lift $ do
         mr <- lookupReminder oid rid
         case mr of
           Just r | r ^. oauthId == oid ->
             (Authorized <$) . assign reminder . Just $ r
           _ -> return . Unauthorized $ "Naggy"
      _ -> return a
  , contentTypesProvided = return [("application/json", withReminder $ return . A.encode)]
  , deleteResource = withSession $ \(oid, _) -> lift . lift $ True <$ deleteReminder oid rid
  }

withReminder :: (Reminder -> HaltT NaggyCrank a) -> HaltT NaggyCrank a
withReminder f = maybe (halt notFound404) f =<< (lift . lift . use) reminder

