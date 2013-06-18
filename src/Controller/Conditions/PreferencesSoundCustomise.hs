-- | Keeps the sound filename, in sync between model and view
module Controller.Conditions.PreferencesSoundCustomise where

-- External imports
import Control.Arrow
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.FileDialog
import Hails.MVC.Controller.ConditionDirection

-- Internal imports
import CombinedEnvironment
import I18N.Strings

-- | Detects changes to the language selection and keeps them in sync with the
-- model
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let (vw, pm) = (view &&& model) cenv

  chkBtn <- preferencesNotebookSoundChkBtn $ mainWindowBuilder vw
  chkBtn `on` toggled $ conditionChkBtn cenv VM
  
  pm `onEvents` events $ conditionChkBtn cenv MV 
  pm `onEvents` events $ conditionSensitive cenv
  pm `onEvents` events $ conditionFilename cenv MV

  btn <- preferencesNotebookSoundBtn $ mainWindowBuilder vw
  btn `onClicked` conditionLoadSound cenv

  entry <- preferencesNotebookSoundEntry $ mainWindowBuilder vw
  entry `onEditableChanged` conditionFilename cenv VM

  where events = [ Initialised, SoundFilenameChanged ]

-- | Keeps the sound notification option in sync
-- (Model <=> View)
conditionChkBtn :: CEnv -> ConditionDirection -> IO()
conditionChkBtn  cenv cd = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv

  -- View
  chkBtn <- preferencesNotebookSoundChkBtn $ mainWindowBuilder vw
  vwValue <- get chkBtn toggleButtonActive

  -- Model
  fn <- getSoundFilename pm
  let pmValue = isJust fn

  -- Condition
  when (vwValue /= pmValue) $
    case cd of
     VM -> setSoundFilename pm (if vwValue then Just "" else Nothing)
     MV -> set chkBtn [ toggleButtonActive := pmValue ]

-- | The text entry is sensitive iff sound notification is enabled
-- (Model => View)
conditionSensitive :: CEnv -> IO()
conditionSensitive cenv = onViewAsync $ do
  -- View
  align   <- preferencesNotebookSoundAlign2 $ mainWindowBuilder $ view cenv
  -- Model
  pmValue <- fmap isJust $ getSoundFilename $ model cenv
  -- Condition
  widgetSetSensitive align pmValue

-- | The file selected by the user is used for sound notification
-- View => Model
conditionLoadSound :: CEnv -> IO ()
conditionLoadSound cenv = onViewAsync $ do
  -- View
  r <- openOpenFileDialog strOpenSoundFile [(["*.wav", "*.WAV", "*.Wav"], strSoundFiles)]
  -- Condition
  when (isJust r) $ setSoundFilename (model cenv) r

-- | The file path entered by the user in the text entry represents the file
-- name of the sound file used for sound notification
-- (View <=> Model)
conditionFilename :: CEnv -> ConditionDirection -> IO()
conditionFilename cenv cd = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv

  -- View
  entry            <- preferencesNotebookSoundEntry $ mainWindowBuilder vw
  t                <- get entry entryText

  -- Model
  curModelValue    <- getSoundFilename pm
  let viewValueShould = fromMaybe "" curModelValue

  -- Condition
  when (t /= viewValueShould) $
   case cd of
     VM -> when (isJust curModelValue) $
             setSoundFilename pm (Just t)
     MV -> set entry [ entryText := viewValueShould ]
