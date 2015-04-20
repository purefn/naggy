{-# LANGUAGE OverloadedStrings #-}

module HipBot.Naggy.Session where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import Data.List (find)
import Data.Maybe
import qualified Data.Serialize as Serial
import qualified Data.Text.Encoding as T
import Network.Wai.Lens
import Web.ClientSession
import Web.Cookie
import Webcrank.Wai

import HipBot
import HipBot.Naggy.Types

writeSession :: Registration -> NaggyCrank ()
writeSession reg = do
  k <- lift . view $ csKey
  let sess = (reg ^. oauthId . to T.encodeUtf8, reg ^. roomId . to fromJust)
  sess' <- liftIO . encryptIO k . Serial.encode $ sess
  putResponseHeader "Set-Cookie" . LB.toStrict . toLazyByteString . renderSetCookie $ def
    { setCookieName = "sid"
    , setCookieValue =  sess'
    , setCookieMaxAge = Just 86400 -- 24 hours
    , setCookieHttpOnly = True
    }

checkAuthorization :: NaggyCrank Authorized
checkAuthorization = do
  sess <- readSession
  lift . assign session $ sess
  return $ maybe (Unauthorized "Naggy") (const Authorized) sess

readSession :: NaggyCrank (Maybe Session)
readSession = runMaybeT $ do
  hdr <- MaybeT . preview $ request . headers . value "Cookie"
  let cs = parseCookies hdr
  c <- maybe mzero (return . snd) (find (("sid" ==) . fst) cs)
  k <- lift . lift . view $ csKey
  hoistMaybe .
    fmap (first T.decodeUtf8) .
    (eitherToMaybe . Serial.decode =<<) $
    decrypt k c

withSession :: (Session -> HaltT NaggyCrank a) -> HaltT NaggyCrank a
withSession f =
  maybe (halt forbidden403) f =<< (lift . lift . use) session

withSession_ :: HaltT NaggyCrank () -> HaltT NaggyCrank ()
withSession_ f = withSession (const f)

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

