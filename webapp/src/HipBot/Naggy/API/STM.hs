module HipBot.Naggy.API.STM where

import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Lens hiding ((&))
import qualified Data.List as List
import Protolude hiding ((<>), to)

import HipBot
import qualified HipBot.API.STM as HipBot
import HipBot.Naggy.API
import HipBot.Naggy.Types

stmAPI :: IO (NaggyAPI IO)
stmAPI = do
  rs <- newTVarIO [] :: IO (TVar [Reminder])
  let
    save r = atomically . modifyTVar' rs $ (r :)
    foldAll f a = atomically . fmap (List.foldl' f a) . readTVar $ rs
    foldR f a oid =
      atomically .
        fmap (List.foldl' f a . filter (\r -> r ^. oauthId == oid)) .
        readTVar $
        rs
    lookupR rid =
      atomically .
        fmap (List.find (\r -> r ^. ident == rid)) .
        readTVar $
        rs
    deleteRs oid =
      atomically . modifyTVar' rs $
        filter (\r -> r ^. oauthId /= oid)
    deleteR rid =
      atomically . modifyTVar' rs $
        filter (\r -> r ^. ident /= rid)

  NaggyAPI save foldAll foldR lookupR deleteRs deleteR <$> HipBot.stmAPI

