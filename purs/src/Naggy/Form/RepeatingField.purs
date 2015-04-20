module Naggy.Form.RepeatingField 
  ( repeatingField
  ) where

import Data.Date
import Data.Enum
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Monad as E
import Optic.Core

import Naggy.Form.AUI
import Naggy.Types

repeatingField
  :: ReminderForm
  -> H.HTML _ (E.Event (NaggyEffects _) Input)
repeatingField (ReminderForm es r@(ReminderData ro)) = build where
  build = case ro.repeating of
    Weekly n ds -> weekly n ds
  weekly n days = H.div 
    [ A.classes [A.className "field-group"] ] $
    [ H.label_ [ H.text "Repeat on" ] ] ++
      (enumFromTo Sunday Saturday <#> dayCheckbox days n) ++
      errorDiv es "weekly"
  dayCheckbox days n day = H.span_ 
    [ H.input
      [ A.classes [A.className "checkbox"]
      , A.type_ "checkbox"
      , A.id_ $ String.toLower $ show day
      , A.checked (Set.member day days)
      , A.onChecked $ A.input $ \checked -> ShowForm $ -- ReminderForm es r
          if checked
            then 
              ReminderForm (deleteFieldError "weekly" es) (r # _ReminderData .. repeating .~ Weekly n (Set.insert day days))
            else 
              let
                days' = Set.delete day days
                es' = if Set.isEmpty days'
                  then insertFieldError "weekly" "At least one day must be selected." es
                  else es
                r' = r # _ReminderData .. repeating .~ Weekly n days'
              in
                ReminderForm es' r'
      ] []
    , H.label 
        [ A.for $ String.toLower $ show day ]
        [ H.text $ String.take 1 $ show day ]
    ]

