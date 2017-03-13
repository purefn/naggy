{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HipBot.Naggy.RequestLogger (logStdoutJSON) where

import qualified Blaze.ByteString.Builder as BB
import Data.Aeson
import Data.Monoid ((<>))
import Data.Default (def)
import Data.IP
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import Network.HTTP.Types as H
import Network.Socket (SockAddr (..), PortNumber)
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (toLogStr)
import Text.Printf (printf)
import System.IO.Unsafe

logStdoutJSON :: Middleware
logStdoutJSON = unsafePerformIO . mkRequestLogger $ def
  { outputFormat = CustomOutputFormatWithDetails formatAsJSON
  }

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration _ response =
  (toLogStr . encode . object $
    [ "method" .= decodeUtf8 (requestMethod req)
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "queryString" .= map queryItemToJSON (queryString req)
    , "durationMs" .= (readAsDouble . printf "%.2f" . rationalToDouble $ toRational duration * 1000)
    , "size" .= requestBodyLengthToJSON (requestBodyLength req)
    , "remoteHost" .= sockToJSON (remoteHost req)
    , "httpVersion" .= httpVersionToJSON (httpVersion req)
    , "status" .= statusCode status
    , "size"   .= responseSize
    , "response" .=
        if statusCode status >= 400
          then Just . decodeUtf8 . BB.toByteString $ response
          else Nothing
    , "time"     .= decodeUtf8 date
    ]) <> "\n"

word32ToHostAddress :: Word32 -> Text
word32ToHostAddress = T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

rationalToDouble :: Rational -> Double
rationalToDouble = fromRational

sockToJSON :: SockAddr -> Value
sockToJSON (SockAddrInet pn ha) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= word32ToHostAddress ha
    ]
sockToJSON (SockAddrInet6 pn _ ha _) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= ha
    ]
sockToJSON (SockAddrUnix sock) =
  object [ "unix" .= sock ]
sockToJSON (SockAddrCan i) =
  object [ "can" .= i ]

queryItemToJSON :: QueryItem -> Value
queryItemToJSON (name, mValue) = toJSON (decodeUtf8 name, fmap decodeUtf8 mValue)

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) = String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l
