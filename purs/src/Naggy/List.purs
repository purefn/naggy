module Naggy.List 
  ( renderList
  ) where

import Control.Functor
import Control.Monad.Aff
import Control.Monad.Eff
import Data.Date
import Data.Date.Locale
import qualified Data.Set as Set
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Monad as E

import Naggy.Ajax
import Naggy.Types

renderList :: [Reminder] -> Boolean -> H.HTML _ (E.Event (NaggyEffects _) Input)
renderList rs loading = H.div_ [ title, reminders_ ] where
  title = H.h1 
    [ A.classes [ A.className "title" ] ] 
    [ H.text "Reminders"
    , if loading then waitIcon else addButton 
    ]
  reminders_ = H.table 
    [ A.classes [A.className "aui"] ] 
    [ H.thead_
        [ H.tr_ 
            [ H.th_ [ H.text "Message" ]
            , H.th_ [ H.text "Time" ]
            , H.th_ [ H.text "Repeating" ]
            , H.th_ [ H.text "Actions" ]
            ]
        ]
    , H.tbody_ (reminder <$> rs)
    ]
  reminder r@(Reminder _ (ReminderData o)) = H.tr_
    [ H.td_ [ H.text o.message ]
    , H.td_ [ H.text (show o.time ++ " " ++ o.tz) ]
    , H.td_ [ H.text $ show o.repeating ]
    , H.td 
        [ A.classes [ A.className "action" ] ] 
        [ H.ul
            [ A.classes [ A.className "menu" ] ]
            [ H.li_
                [ H.a 
                    [ A.onclick $ const $ pure (DeleteReminder r <$ E.async (deleteReminder r))
                    , A.href "#"
                    ]
                    [ H.span 
                        [ A.classes $ A.className <$> [ "aui-icon", "aui-icon-small", "aui-iconfont-delete" ] ]
                        [ H.text "Delete" ]
                    ]
                ]
            ]
        ]
    ]
  waitIcon = H.span
    [ A.classes (A.className <$> [ "aui-icon", "aui-icon-small", "aui-icon-wait" ]) ]
    [ H.text "Loading..." ]
  addButton =
    H.button
      [ A.onclick $ const $ pure (ShowForm <<< ReminderForm emptyFormErrors <$> newReminder) 
      , A.classes (A.className <$> [ "aui-button", "aui-button-primary" ])
      ]
      [ H.text "Add" ]

newReminder :: forall eff. E.Event (NaggyEffects eff) ReminderData
newReminder = E.async $ makeAff $ \_ k -> do
  n <- now 
  h <- hourOfDay n
  m <- minuteOfHour n
  tz <- currentTzName
  d <- dayOfWeek n
  k $ ReminderData
    { time: Time { hour: h, minute: m }
    , tz: tz
    , repeating: Weekly 1 (Set.singleton d)
    , message: ""
    }

foreign import currentTzName
  """
  function currentTzName() {
    return jstz.determine().name();
  }
  """ :: forall e. Eff (locale :: Locale | e) String

