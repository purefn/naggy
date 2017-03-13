module HipBot.Internal.JSON where

import Data.Aeson.Types
import Data.Char

trailingFieldName :: Int -> String -> String
trailingFieldName n = (\(x:xs) -> toLower x:xs) . drop n

deriveJSONOptions :: Int -> Options
deriveJSONOptions n =
  defaultOptions
    { fieldLabelModifier = trailingFieldName n
    , omitNothingFields = True
    }

