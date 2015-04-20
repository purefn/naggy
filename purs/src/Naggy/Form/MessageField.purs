module Naggy.Form.MessageField 
  ( messageField
  ) where

import qualified Data.String as String
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Monad as E
import Optic.Core

import Naggy.Form.AUI
import Naggy.Types

messageField
  :: ReminderForm
  -> H.HTML _ (E.Event (NaggyEffects _) Input)
messageField (ReminderForm es r@(ReminderData ro)) = H.div 
  [ A.classes [A.className "field-group"] ] $
  [ H.label [ A.for "message" ] [ H.text "Message" ]
  , H.input 
      [ A.classes (A.className <$> [ "text", "long-field" ])
      , A.id_ "message"
      , A.value ro.message
      , A.onInput $ A.input $ \msg ->
          let
            form' = if String.length msg > 0
              then
                ReminderForm (deleteFieldError "message" es) (r # _ReminderData .. message .~ msg)
              else
                ReminderForm (insertFieldError "message" "Message cannot be empty" es) r
          in
            ShowForm form'
      ] []
  ] ++ errorDiv es "message"

