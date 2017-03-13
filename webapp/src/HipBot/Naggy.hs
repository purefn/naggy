{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module HipBot.Naggy where

import Control.Lens hiding ((.=), Context)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), MonadError)
import Control.Monad.Logger (MonadLogger, LoggingT(..), Loc(..), logError)
import Crypto.Random (MonadRandom(..))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lucid
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort, setOnException, runSettings)
import Protolude hiding (to)
import Servant
import Servant.HTML.Lucid
import System.Log.FastLogger (fromLogStr)

import HipBot
import HipBot.Http
import qualified HipBot.Naggy.API as API
import HipBot.Naggy.Config
import HipBot.Naggy.ConfigHtml
import HipBot.Naggy.Env
import HipBot.Naggy.RequestLogger
import HipBot.Naggy.Types

type Html' = Html ()

newtype Naggy a = Naggy { unNaggy :: ExceptT ServantErr (ReaderT (NaggyEnv Naggy) (LoggingT IO)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadError ServantErr
    , MonadIO
    , MonadLogger
    , MonadReader (NaggyEnv Naggy)
    , MonadThrow
    )

instance MonadRandom Naggy where
  getRandomBytes = liftIO . getRandomBytes

runJSONLoggingT :: LoggingT m a -> m a
runJSONLoggingT =
  let
    json loc src lvl str = putStrLn . A.encode . A.object $
      [ "location" .= mconcat
          [ loc_package loc
          , ":"
          , loc_module loc
          , " "
          , loc_filename loc
          , ":"
          , show . fst . loc_start $ loc
          , ":"
          , show . snd . loc_start $ loc
          ]
      , "src" .=
          if T.null src
            then Nothing
            else Just src
      , "level" .= (show lvl :: [Char])
      , "message" .= T.decodeUtf8 (fromLogStr str)
      ]
  in (`runLoggingT` json)

runNaggy :: Naggy a -> NaggyEnv Naggy -> ExceptT ServantErr IO a
runNaggy h = ExceptT . runJSONLoggingT . runReaderT (runExceptT . unNaggy $ h)

runNaggyNT :: NaggyEnv Naggy -> (Naggy :~> ExceptT ServantErr IO)
runNaggyNT e = Nat (`runNaggy` e)

naggyServerCtx :: NaggyEnv Naggy -> Context (JwtAuthHandler ': '[])
naggyServerCtx env = jwtAuthHandler (runNaggyNT env) :. EmptyContext

naggyApp :: NaggyEnv Naggy -> Application
naggyApp env = serveWithContext naggyHttpAPI (naggyServerCtx env) (naggyServer env)

type NaggyHttpAPI
  = "healthcheck" :> Get '[PlainText] Text
  :<|> "configure" :> JwtAuth :> Get '[HTML] Html'
  :<|> "reminders" :> JwtAuth :> Get '[JSON] [Reminder]
  :<|> "reminders" :> JwtAuth :> ReqBody '[JSON] ReminderInfo :> PostCreated '[JSON] Reminder
  :<|> "reminders" :> Capture "reminderId" ReminderId :> JwtAuth :> DeleteNoContent '[JSON] NoContent
  :<|> HipBotHttpAPI

naggyHttpAPI :: Proxy NaggyHttpAPI
naggyHttpAPI = Proxy

naggyServerT :: ServerT NaggyHttpAPI Naggy
naggyServerT
  = getHealthcheck
  :<|> configure
  :<|> getReminders
  :<|> addReminder
  :<|> deleteReminder
  :<|> hipBotServerT

naggyServer :: NaggyEnv Naggy -> Server NaggyHttpAPI
naggyServer env = enter (runNaggyNT env) naggyServerT

configure :: OAuthClient -> Naggy Html'
configure _ = pure configHtml

getReminders :: OAuthClient -> Naggy [Reminder]
getReminders = API.lookupReminders . view oauthId

addReminder :: OAuthClient -> ReminderInfo -> Naggy Reminder
addReminder client reminder =
  let
    err = throwError err400 { errBody = "No room associated with this installation." }
    save room = API.saveReminder (client ^. oauthId) room reminder
  in
    maybe err save (client ^. roomId)

deleteReminder :: ReminderId -> OAuthClient -> Naggy NoContent
deleteReminder rid client =
  let
    lookup = maybe (throwError err404) pure =<< API.lookupReminder rid
    checkPerm r = when (r ^. oauthId /= client ^. oauthId) (throwError err403)
    checkr = checkPerm =<< lookup
  in
    NoContent <$ API.deleteReminder rid <* checkr

getHealthcheck :: Naggy Text
getHealthcheck = pure "OK"

run :: IO ()
run = do
  config <- readConfig
  env <- initNaggyEnv config
  runSettings (settings config) (logStdoutJSON . naggyApp $ env)

settings :: Config -> Settings
settings config =
  setPort (config ^. port . to fromIntegral) .
    setOnException (const logException) $
    defaultSettings

logException :: SomeException -> IO ()
logException e = runJSONLoggingT ($(logError) (show e))

