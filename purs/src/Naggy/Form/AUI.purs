module Naggy.Form.AUI where

import Data.Maybe
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events.Monad as E

import Naggy.Types

errorDiv :: FormErrors -> String -> [H.HTML _ (E.Event (NaggyEffects _) Input)]
errorDiv es field = maybe [] pure (lookupFieldError field es) <#> \e ->
  H.div [ A.classes [A.className "error"] ] [ H.text e ]

