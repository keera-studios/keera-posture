module Controller.Conditions.PreferencesSoundCustomise where

-- External imports
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Sequence as Seq
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Helpers.FileDialog
import Hails.I18N.Gettext
import Hails.MVC.Controller.ConditionDirection

-- Internal imports
import CombinedEnvironment

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

  where events = Seq.fromList [ Initialised, SoundFilenameChanged]

conditionChkBtn :: CEnv -> ConditionDirection -> IO()
conditionChkBtn  cenv cd = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv

  chkBtn <- preferencesNotebookSoundChkBtn $ mainWindowBuilder vw
  vwValue <- get chkBtn toggleButtonActive

  fn <- getSoundFilename pm
  let pmValue = isJust fn

  when (vwValue /= pmValue) $
    case cd of
     VM -> setSoundFilename pm (if vwValue then Just "" else Nothing)
     MV -> set chkBtn [ toggleButtonActive := pmValue ]

conditionSensitive :: CEnv -> IO()
conditionSensitive cenv = onViewAsync $ do
  align   <- preferencesNotebookSoundAlign2 $ mainWindowBuilder $ view cenv
  pmValue <- fmap isJust $ getSoundFilename $ model cenv
  widgetSetSensitive align pmValue

conditionLoadSound :: CEnv -> IO ()
conditionLoadSound cenv = onViewAsync $ do
  r <- openOpenFileDialog (__ "Open sound file" ) [(["*.wav", "*.WAV", "*.Wav"], __ "WAV Sound files")]
  when (isJust r) $ setSoundFilename (model cenv) r

conditionFilename :: CEnv -> ConditionDirection -> IO()
conditionFilename cenv cd = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv
  entry            <- preferencesNotebookSoundEntry $ mainWindowBuilder vw
  t                <- get entry entryText
  curModelValue    <- getSoundFilename pm
  let viewValueShould = fromMaybe "" curModelValue

  when (t /= viewValueShould) $
   case cd of
     VM -> when (isJust curModelValue) $
             setSoundFilename pm (Just t)
     MV -> set entry [ entryText := viewValueShould ]
