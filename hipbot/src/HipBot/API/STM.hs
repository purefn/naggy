module HipBot.API.STM
  ( stmAPI
  , module HipBot.API
  ) where

import Prelude
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap

import HipBot.API
import HipBot.Types

stmAPI :: MonadIO m => IO (HipBotAPI m)
stmAPI = do
  regs <- newTVarIO HashMap.empty
  let
    insert r t =
      liftIO .
        atomically .
        modifyTVar' regs .
        HashMap.insert (r ^. oauthId) $
        (r, t)
    delete =
      liftIO .
        atomically .
        modifyTVar' regs .
        HashMap.delete
    lookupR =
      liftIO .
        atomically .
        flip fmap (readTVar regs) .
        HashMap.lookup
    update oid t =
      liftIO .
        atomically .
        modifyTVar' regs .
        HashMap.adjust (set _2 t) $
        oid

  pure $ HipBotAPI insert delete lookupR update

