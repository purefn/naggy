{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Naggy.Env where

import Control.Lens
import Control.Monad.Logger (MonadLogger)
import Data.Pool (createPool)
import Database.PostgreSQL.Simple as PSQL (connectPostgreSQL, close)
import Protolude

import HipBot
import HipBot.Naggy.API.PG as Naggy
import HipBot.Naggy.Config
import HipBot.Naggy.Descriptor

data NaggyEnv m = NaggyEnv
  { _naggyEnvNaggyAPI :: NaggyAPI m
  , _naggyEnvHipBot :: HipBot m
  }

makeClassy ''NaggyEnv

instance HasNaggyAPI (NaggyEnv m) m where
  naggyAPI = naggyEnvNaggyAPI

instance HasHipBotAPI (NaggyEnv m) m where
  hipBotAPI = naggyEnvHipBot . hipBotBotAPI

instance HasHipBot (NaggyEnv m) m where
  hipBot = naggyEnvHipBot

initNaggyEnv :: (MonadIO m, MonadLogger m, MonadReader r m, HasNaggyAPI r m) => Config -> IO (NaggyEnv m)
initNaggyEnv config = do
  pool <- createPool (connectPostgreSQL . view db $ config) PSQL.close 1 10 10
  api <- Naggy.pgAPI pool
  bot <- newHipBot api (descriptor . view baseUri $ config) deleteReminders
  pure . NaggyEnv api $ bot

