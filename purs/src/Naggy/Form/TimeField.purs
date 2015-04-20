module Naggy.Form.TimeField 
  ( timeField 
  ) where

import Data.Int (Int(..), fromNumber, toNumber)
import Data.Maybe
import qualified Data.String as String
import Data.Time
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Monad as E
import Optic.Core

import Naggy.Form.AUI
import Naggy.Types

timeField
  :: ReminderForm
  -> H.HTML _ (E.Event (NaggyEffects _) Input)
timeField (ReminderForm es r@(ReminderData ro)) = H.div 
  [ A.classes [A.className "field-group"] ] $
  [ H.label [ A.for "time" ] [ H.text "Time" ]
  , H.input 
      [ A.classes (A.className <$> [ "time" ])
      , A.type_ "time"
      , A.id_ "time"
      , A.value $ show ro.time
      , A.onInput $ A.input $ \s ->
          let
            parse :: forall a. (Number -> Boolean) -> (Int -> a) -> String -> Maybe a
            parse tf f s = parseInt s >>= \n ->
              if tf n
                then Just (f (fromNumber n))
                else Nothing
            mt = case String.split ":" s of 
              [h, m] -> mkTime 
                <$> parse (\n -> n >= 0 && n <= 23) HourOfDay h
                <*> parse (\n -> n >= 0 && n <= 59) MinuteOfHour m
              _ -> Nothing
            form' = case mt of
              Just t ->
                ReminderForm (deleteFieldError "time" es) (r # _ReminderData .. time .~ t)
              Nothing ->
                ReminderForm (insertFieldError "time" "Time must be in 24-hour format, hh:mm" es) r
          in 
            ShowForm form'
      ] []
  , H.select
      [ A.classes (A.className <$> [ "select" ])
      , A.value ro.tz
      , A.onValueChanged $ A.input \tz' -> 
          ShowForm $ ReminderForm es (r # _ReminderData .. tz .~ tz')
      ] $
      timezones unit <#> \tz -> H.option
        [ A.selected $ tz == ro.tz ]
        [ H.text tz ]
  ] ++ errorDiv es "hour" ++ errorDiv es "minute"

foreign import parseInt
  """
  function parseInt(s) {
    if(/^([0-9]+)$/.test(s)) return new Data_Maybe.Just(Number(s));
    else return Data_Maybe.Nothing.value;
  }
  """ :: String -> Maybe Number

foreign import timezones
  """
  function timezones() {
    return JSON.parse(document.getElementById('timezones').innerHTML);
  }
  """ :: Unit -> [String]

