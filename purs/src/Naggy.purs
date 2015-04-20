module Naggy where

import Control.Monad.Aff
import Control.Monad.Eff
import Data.Array (filter)
import Data.Tuple
import Debug.Trace
import DOM
import Halogen
import Halogen.Component
import Halogen.Signal
import qualified Halogen.HTML.Events.Monad as E
import Network.HTTP.Affjax

import Naggy.Ajax
import Naggy.Form
import Naggy.List
import Naggy.Types

ui :: Component _ (E.Event (NaggyEffects _)) Input Input
ui = component (render <$> stateful initialState update) where
  render (State o) = case o.displaying of
    Loading -> renderList o.reminders true
    List -> renderList o.reminders false
    Form form -> renderForm form 
  update (State o) input = case input of
    ShowList rs -> State o { displaying = List, reminders = rs }
    ShowForm form -> State o { displaying = Form form }
    AddReminder r -> State o { displaying = List, reminders = r : o.reminders }
    DeleteReminder (Reminder ident _) -> 
      let pred (Reminder ident' _) = ident' /= ident
      in State o { reminders = filter pred o.reminders }
    CancelAdd -> State o { displaying = List }

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  runAff (trace <<< show) (driver <<< ShowList) loadReminders

foreign import appendToBody
  """
  function appendToBody(node) {
    return function() {
      document.body.appendChild(node);
    };
  }
  """ :: forall eff. Node -> Eff (dom :: DOM | eff) Node

