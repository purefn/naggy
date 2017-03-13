{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens hiding ((&))
import Data.Aeson.Types (parseMaybe, parseJSON)
import Data.JSString (JSString)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), ZonedTime(..), getZonedTime)
import Data.Time.Zones.All (TZLabel(Etc__UTC), fromTZName, toTZName)
import Prelude (read)
import Protolude hiding (to)
import React.Flux

import TZFFI
import Types

data ReminderDisplay
  = ReminderList
  | NewReminderForm ReminderInfo
  | UpdateReminderForm Reminder
  deriving Show

data ReminderState = ReminderState
  { _reminderStateDisplay :: ReminderDisplay
  , _reminderStateReminders :: [Reminder]
  } deriving Show

makeFields ''ReminderState

data ReminderListArgs = ReminderListArgs
  { _reminderListArgsReminders :: [Reminder]
  , _reminderListArgsOnAddClicked :: [SomeStoreAction]
  , _reminderListArgsOnEditClicked :: Reminder -> [SomeStoreAction]
  , _reminderListArgsOnDeleteClicked :: ReminderId -> [SomeStoreAction]
  }

makeFields ''ReminderListArgs

data ReminderFormArgs = ReminderFormArgs
  { _reminderFormArgsOnSubmit :: ReminderInfo -> [SomeStoreAction]
  }

makeFields ''ReminderFormArgs

data TimeInputArgs a = TimeInputArgs
  { _timeInputArgsOnBlurHandler :: TimeOfDay -> ([SomeStoreAction], Maybe a)
  }

makeFields ''TimeInputArgs

data CheckboxArgs a = CheckboxArgs
  { _checkboxArgsName :: Text
  , _checkboxArgsOnChangeHandler :: Bool -> ([SomeStoreAction], Maybe a)
  }

makeFields ''CheckboxArgs

data MessageInputArgs a = MessageInputArgs
  {_messageInputArgsOnBlurHandler :: Text -> ([SomeStoreAction], Maybe a)
  }

makeFields ''MessageInputArgs

data TZSelectArgs a = TZSelectArgs
  {_tZSelectArgsOnChangeHandler :: TZLabel -> ([SomeStoreAction], Maybe a)
  }

makeFields ''TZSelectArgs

main :: IO ()
main = reactRender "naggy" naggyApp ()

naggyApp :: ReactView ()
naggyApp = defineControllerView "naggy app" reminderStore $ \rs _ ->
  case rs ^. display of
    ReminderList ->
      let
        onAdd = dispatch ShowAddReminderForm
        onEdit = dispatch . ShowUpdateReminderForm
        onDelete = dispatch . DeleteReminder
        args = ReminderListArgs (rs ^. reminders) onAdd onEdit onDelete
      in
        remindersList_ "reminder-list" args
    NewReminderForm r ->
      let
        onSubmit = dispatch . AddReminder
        args = ReminderFormArgs onSubmit
      in
        reminderForm_ "new-reminder-form" r args
    UpdateReminderForm (Reminder rid rinfo) ->
      let
        onSubmit = dispatch . UpdateReminder . Reminder rid
        args = ReminderFormArgs onSubmit
      in
        reminderForm_ "update-reminder-form" rinfo args

remindersList_ :: JSString -> ReminderListArgs -> ReactElementM h ()
remindersList_ !k !args = viewWithSKey remindersList k args mempty

remindersList :: ReactView ReminderListArgs
remindersList = defineView "reminder-list" $ \_args ->
  div_ $ do
    remindersHeader_ & keyed "reminder-list-header"

remindersHeader_ :: ReactElementM ViewEventHandler ()
remindersHeader_ =
  div_ $ do
    h1_ "Reminders" & keyed "reminder-list-header-title"
    clbutton_ "aui-button aui-button-primary" (dispatch ShowAddReminderForm) "Add" & keyed "reminder-list-header-add-button"

data FormArgs = FormArgs
  { formArgsOnSave :: ReminderInfo -> [ SomeStoreAction ]
  }

fieldGroup :: ReactElementM ViewEventHandler () -> ReactView ()
fieldGroup !e = defineView "field-group" $ \_ ->  div_ [ classNames' [ "field-group" ] ] e

fieldGroup_ :: JSString -> ReactElementM ViewEventHandler () -> ReactElementM h ()
fieldGroup_ !k !e = viewWithSKey (fieldGroup e) k () mempty

reminderForm_ :: JSString -> ReminderInfo -> ReminderFormArgs -> ReactElementM h ()
reminderForm_ !k !r !args = viewWithSKey (reminderForm r) k args mempty

reminderForm :: ReminderInfo -> ReactView ReminderFormArgs
reminderForm init = defineStatefulView "reminder form" init $ \cur args ->
  let
    -- mkText r = r & notification %~ \case
    --   HtmlNotification m -> TextNotification m
    --   a@(TextNotification _) -> a
    -- mkHtml r = r & notification %~ \case
    --   TextNotification m -> HtmlNotification m
    --   a@(HtmlNotification _) -> a
    formEl =
      form_ [ classNames' [ "aui" ] ] $ do
      -- [ fromMaybe [] $ lookupSubmissionError es <#> \e ->
      --     [ H.div
      --       [ A.classes (A.className <$> [ "aui-message", "aui-message-error" ]) ]
      --       [ H.p [ A.classes [ A.className "title" ] ] [ H.strong_ [ H.text "Error!" ] ]
      --       , H.p_ [ H.text "Error while submitting form, try again later." ]
      --       ]
      --     ]
        fieldGroup_ "time-field-group" $ do
          label_ [ "htmlFor" $= "time" ] "Time" & keyed "time-label"
          timeInput_ "time-input" (cur ^. timeOfDay) $
            TimeInputArgs ()
          tzSelect_ "tz-select" (cur ^. tz) $
            TZSelectArgs (dispatch . UpdateFormReminder . (tz .~))

        fieldGroup_ "repeating-field-group" $ do
          label_ "Repeat on" & keyed "repeating-label"
          case cur ^. repeating of
            Weekly _ days ->
              forM_ (enumFromTo Sunday Saturday) $ \d ->
                let
                  f = bool Set.delete Set.insert
                  g checked (Weekly n days') = Weekly n (f checked d days')
                  h = dispatch . UpdateFormReminder . (repeating %~) . g
                  lbl =  showWeekDay d
                  cname = showWeekDay d
                in span_ $ do
                  checkbox_ ("checkbox-" `mappend` showWeekDay d) (Set.member d days) $
                    CheckboxArgs cname h
                  label_ [ "htmlFor" @= cname ] lbl & keyed "html-checkbox-label"

        fieldGroup_ "message-type-field-group" $ do
          div_ [ classNames' [ "checkbox" ] ] $ do
            checkbox_ "html-checkbox" (isHtmlNotification (cur ^. notification)) $
              CheckboxArgs "html" (dispatch . UpdateFormReminder . bool mkText mkHtml)
            label_ [ "htmlFor" $= "html" ] "HTML" & keyed "html-checkbox-label"

        fieldGroup_ "message-text-field-group" $ do
          label_ [ "htmlFor" $= "message" ] "Message" & keyed "message-text-label"
          messageInput_ "message-text-input" (cur ^. message) $
            MessageInputArgs (dispatch . UpdateFormReminder . (message .~))

    buttons =
      let
        addAttrs =
          [ classNames' [ "aui-button", "aui-button-primary" ]
          , if T.length (cur ^. message) > 0
              then onClick $ \_ _ -> dispatch SubmitReminder
              else "disabled" $= "true"
          ]
        cancelAttrs =
          [ classNames' [ "aui-button", "aui-button-link" ]
          , onClick $ \_ _ -> dispatch FormCancel
          ]
      in p_ [ classNames' [ "aui-buttons" ] ] $ do
        button_ addAttrs (maybe "Add" (const "Update") (cur ^. reminderId))
        button_ cancelAttrs "Cancel"
  in
    div_ $ do
      h1_ [ classNames' [ "title" ] ] "Add reminder" & keyed "reminder-form-title"
      formEl & keyed "reminder-form-form"
      buttons & keyed "reminder-form-buttons"

tzSelect_ :: JSString -> TZLabel -> TZSelectArgs a -> ReactElementM h ()
tzSelect_ !k !z !args = viewWithSKey (tzSelect z) k args mempty

tzSelect :: TZLabel -> ReactView (TZSelectArgs a)
tzSelect z = defineStatefulView "reminder form tz select" z $ \cur args ->
  let
    attrs =
      [ classNames' [ "select" ]
      , "value" &= fromEnum cur
      , onChange $ \e _ ->
           let value = toEnum . read . target e $ "value"
           in ((args ^.onChangeHandler) value, Just value)
      ]

    tzs :: [TZLabel]
    tzs = [minBound..maxBound]

    option :: TZLabel -> ReactElementM h ()
    option o = option_ [ "value" &= fromEnum o ] (elemText . T.decodeUtf8 . toTZName $ o)

    options = traverse_ option tzs
  in select_ attrs options

messageInput_ :: JSString -> Text -> MessageInputArgs -> ReactElementM h ()
messageInput_ !k !t !args = viewWithSKey (messageInput t) k args mempty

messageInput :: Text -> ReactView MessageInputArgs
messageInput init = defineStatefulView "reminder form message input" init $ \cur args ->
  input_
    [ classNames' [ "text", "long-field" ]
    , "placeholder" $= "What do you want to be reminded about?"
    , "value" @= cur
    , onChange $ \e _ -> ([], Just . target e $ "value")
    , onBlur $ \_ _ t -> ((args ^. onBlurHandler) t, Just t)
    ]

checkbox_ :: JSString -> Bool -> CheckboxArgs -> ReactElementM h ()
checkbox_ !k !c !args = viewWithSKey (checkbox c) k args mempty

checkbox :: Bool -> ReactView CheckboxArgs
checkbox init = defineStatefulView "reminder form checkbox" init $ \cur args ->
  input_
    [ classNames' [ "checkbox" ]
    , "type" $= "checkbox"
    , "checked" @= cur
    , onChange $ \_ s ->
        let v = not s
        in ((args ^. onChangeHandler) v, Just $ v)
    ]

timeInput_ :: JSString -> TimeOfDay -> TimeInputArgs -> ReactElementM h ()
timeInput_ !k !t !args = viewWithSKey (timeInput t) k args mempty

-- TODO error handling?
timeInput :: TimeOfDay -> ReactView TimeInputArgs
timeInput init =
  let
    parseValue = parseMaybe parseJSON . flip target "value"
    onBlur' = (^. onBlurHandler)
  in
    defineStatefulView "reminder form time field" init $ \cur args ->
      input_
        [ classNames' [ "time" ]
        , "type" $= "time"
        , "value" @= cur
        , onChange $ \e _ -> ([], parseValue e)
        , onBlur $ \_ _ t -> (onBlur' args t, Just t)
        ]
  --     , A.onInput $ A.input $ \s ->
  --         let
  --           parse :: forall a. (Number -> Boolean) -> (Int -> a) -> String -> Maybe a
  --           parse tf f s = parseInt s >>= \n ->
  --             if tf n
  --               then Just (f (fromNumber n))
  --               else Nothing
  --           mt = case String.split ":" s of
  --             [h, m] -> mkTime
  --               <$> parse (\n -> n >= 0 && n <= 23) HourOfDay h
  --               <*> parse (\n -> n >= 0 && n <= 59) MinuteOfHour m
  --             _ -> Nothing
  --           form' = case mt of
  --             Just t ->
  --               ReminderForm (deleteFieldError "time" es) (r # _ReminderData .. time .~ t)
  --             Nothing ->
  --               ReminderForm (insertFieldError "time" "Time must be in 24-hour format, hh:mm" es) r
  --         in
  --           ShowForm form'
  --     ] []
  -- , H.select
  --     [ A.classes (A.className <$> [ "select" ])
  --     , A.value ro.tz
  --     , A.onValueChanged $ A.input \tz' ->
  --         ShowForm $ ReminderForm es (r # _ReminderData .. tz .~ tz')
  --     ] $
  --     timezones unit <#> \tz -> H.option
  --       [ A.selected $ tz == ro.tz ]
  --       [ H.text tz ]
  -- ] ++ errorDiv es "hour" ++ errorDiv es "minute"

data ReminderAction
  = ShowAddReminderForm
  | ShowUpdateReminderForm Reminder
  | AddReminder ReminderInfo
  | UpdateReminder Reminder
  | DeleteReminder ReminderId
  | FormCancel
  deriving (Show, Generic, NFData)

instance StoreData ReminderState where
  type StoreAction ReminderState = ReminderAction

  transform action rs = do
    putStrLn ("Action: " <> showAction action :: Text)
    putStrLn ("State: " <> show rs :: Text)

    rs' <- case action of
      ShowAddReminderForm -> do
        now <- getZonedTime
        z <- getCurrentTZ
        let
          d = (\(_, _, i) -> toEnum (i - 1)) . toWeekDate . localDay . zonedTimeToLocalTime $ now
          repeats = Weekly 1 (Set.singleton d)
          zeroSeconds (TimeOfDay h m _) = TimeOfDay h m 0
          time = zeroSeconds . localTimeOfDay . zonedTimeToLocalTime $ now
          parseTZ = fromMaybe Etc__UTC . fromTZName
          r = ReminderInfo time (parseTZ z) repeats (TextNotification "")
        pure $ rs & form ?~ ReminderForm Nothing r
      ShowUpdateReminderForm r ->
        pure $ rs & form ?~ ReminderForm (Just $ r ^. ident) (r ^. info)
      AddReminder r ->
        notImplemented
      UpdateReminder rid r ->
        notImplemented
      _ -> notImplemented

    putStrLn ("Updated State: " <> show rs' :: Text)

    pure rs'

reminderStore :: ReactStore ReminderState
reminderStore = mkStore . ReminderState Nothing $ []

dispatch :: ReminderAction -> [SomeStoreAction]
dispatch a = [ SomeStoreAction reminderStore a ]

keyed :: JSString -> ReactElementM ViewEventHandler () -> ReactElementM h ()
keyed k e = viewWithSKey (defineView k (const e)) k () mempty

classNames' :: [Text] -> PropertyOrHandler h
classNames' = classNames . fmap (flip (,) True)

