{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Exception
import qualified Data.ByteString.UTF8 as B
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Lens
import Network.Wai.Middleware.RequestLogger
import Safe
import System.Environment (lookupEnv)
import Webcrank.Wai

import HipBot
import HipBot.API as HipBot
import HipBot.Naggy.API as Naggy
import HipBot.Naggy.ConfigPage
import HipBot.Naggy.Descriptor
import HipBot.Naggy.Resources
import HipBot.Naggy.Scheduling
import HipBot.Naggy.Types

import Paths_naggy

main :: IO ()
main = do
  (baseUri, port, db) <- config
  conn <- connectPostgreSQL db
  bot <- newHipBot (HipBot.pgAPI conn) (descriptor baseUri) deleteReminders
  dat <- initialNaggyData (Naggy.pgAPI conn) bot
  runNaggy (forAllReminders startReminder) dat
  putStrLn $ "Starting on port " <> show port <> " with base URI " <> show baseUri
  run dat baseUri port $ mconcat
    [ hipBotResources bot
    , "configure" ==> configResource bot configPage
    , "reminders" ==> remindersResource
    , "reminders" </> param ==> reminderResource
    ]

config :: IO (AbsoluteURI, Int, B.ByteString)
config = (,,) <$> baseUri <*> port <*> db where
  baseUri = fromMaybe "http://localhost:8080" . (>>= parseAbsoluteURI) <$> lookupEnv "BASE_URI"
  port = fromMaybe 8080 . (>>= readMay) <$> lookupEnv "PORT"
  db = maybe (error "DATABASE_URL environment variable required") B.fromString
    <$> lookupEnv "DATABASE_URL"

run :: NaggyData -> AbsoluteURI -> Int -> Dispatcher (WaiResource Naggy) -> IO ()
run dat baseUri port = Warp.run port . logStdout . serveStatic . disp where
  disp = dispatch (`runNaggy` dat) baseUriBS
  baseUriBS = B.fromString . show $ baseUri

serveStatic :: Wai.Middleware
serveStatic app req respond = case req ^. pathInfo of
  "s" : p -> do
    f <- getStaticFilePath p
    respond $ maybe (Wai.responseLBS notFound404 [] "") (\fp -> Wai.responseFile ok200 [] fp Nothing) f
  _ -> app req respond

getStaticFilePath :: [T.Text] -> IO (Maybe FilePath)
getStaticFilePath p =
  let
    p' = T.unpack . mconcat . List.intersperse "/" $ p
    fn = Just <$> getDataFileName p'
  in fn `catch` (pure . ioExceptionAsNothing)

ioExceptionAsNothing :: IOException -> Maybe a
ioExceptionAsNothing = const Nothing

