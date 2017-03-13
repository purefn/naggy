{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module TZFFI where

import qualified Data.JSString as S
import qualified Data.ByteString.Char8 as B
import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "jstz.determine().name()" getCurrentTZJS :: IO JSString

getCurrentTZ :: IO B.ByteString
getCurrentTZ = B.pack . S.unpack <$> getCurrentTZJS
#else
getCurrentTZJS = error "getCurrentTZJS only available from JavaScript"

getCurrentTZ = error "getCurrentTZ only available from JavaScript"
#endif

