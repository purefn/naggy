module Naggy.Form
  ( renderForm
  ) where

import Control.Monad.Aff
import Data.Either
import Data.Foldable
import Data.Maybe
import qualified Data.String as String
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Monad as E
import Network.HTTP.Affjax
import Optic.Core

import Naggy.Ajax
import Naggy.Types
import Naggy.Form.TimeField
import Naggy.Form.RepeatingField
import Naggy.Form.MessageField

renderForm
  :: ReminderForm
  -> H.HTML _ (E.Event (NaggyEffects _) Input)
renderForm form@(ReminderForm es r@(ReminderData ro)) = H.div_ [ title, fields, buttons ] where
  title = H.h1 [ A.classes [A.className "title"] ] [ H.text "Add reminder" ]
  fields = H.form
    [ A.classes [A.className "aui"] ] $
    mconcat 
      [ fromMaybe [] $ lookupSubmissionError es <#> \e -> 
          [ H.div
            [ A.classes (A.className <$> [ "aui-message", "aui-message-error" ]) ]
            [ H.p [ A.classes [ A.className "title" ] ] [ H.strong_ [ H.text "Error!" ] ]
            , H.p_ [ H.text "Error while submitting form, try again later." ]
            ]
          ]
      , [ timeField form
        , repeatingField form
        , typeField form
        , messageField form
        ]
      ]
  buttons = H.p
    [ A.classes [ A.className "aui-buttons" ] ]
    [ let
        handlers = 
          if not (hasFieldErrors es) && String.length (ro.notification ^. message) > 0
            then [ A.onclick $ const $ pure $ addReminder r ]
            else [ A.disabled true ]
      in
        H.button
          ([ A.classes (A.className <$> [ "aui-button", "aui-button-primary" ])] ++ handlers)
          [ H.text "Add" ]
    , H.button
        [ A.classes (A.className <$> [ "aui-button", "aui-button-link" ])
        , A.onclick $ A.input $ const CancelAdd
        ]
        [ H.text "Cancel" ]
    ]

addReminder :: ReminderData -> E.Event (NaggyEffects _) Input
addReminder d = E.async $ do
  e <- attempt $ postReminder d
  return $ case e of
    Left err -> ShowForm $ ReminderForm (insertSubmissionError (show err) emptyFormErrors) d
    Right r -> AddReminder r

