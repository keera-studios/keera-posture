module Controller.Conditions.PreferencesSoundCustomise where

-- External imports
import Control.Arrow
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
import Graphics.UI.Gtk
import Hails.MVC.Controller.ConditionDirection

-- Internal imports
import CombinedEnvironment
import Gettext
import Graphics.UI.Gtk.Helpers.FileDialog

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let (vw, pm) = (view &&& model) cenv

  chkBtn <- preferencesNotebookSoundChkBtn $ mainWindowBuilder vw
  chkBtn `on` toggled $ conditionChkBtn cenv VM
  
  pm `onEvent` Initialised $ conditionChkBtn cenv MV 
  pm `onEvent` SoundFilenameChanged $ conditionChkBtn cenv MV 

  pm `onEvent` Initialised          $ conditionSensitive cenv
  pm `onEvent` SoundFilenameChanged $ conditionSensitive cenv

  btn <- preferencesNotebookSoundBtn $ mainWindowBuilder vw
  btn `onClicked` conditionLoadSound cenv

  entry <- preferencesNotebookSoundEntry $ mainWindowBuilder vw
  entry `onEditableChanged` conditionFilename cenv VM

  pm `onEvent` Initialised          $ conditionFilename cenv MV
  pm `onEvent` SoundFilenameChanged $ conditionFilename cenv MV

conditionChkBtn :: CEnv -> ConditionDirection -> IO()
conditionChkBtn  cenv cd = onViewAsync $ do
  -- (vw, pm) <- fmap (view &&& model) $ readIORef cenv
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
  -- (vw, pm) <- fmap (view &&& model) $ readIORef cenv
  let (vw, pm) = (view &&& model) cenv

  align   <- preferencesNotebookSoundAlign2 $ mainWindowBuilder vw
  pmValue <- fmap isJust $ getSoundFilename pm

  widgetSetSensitive align pmValue

conditionLoadSound :: CEnv -> IO ()
conditionLoadSound cenv = onViewAsync $ do
  r <- openOpenFileDialog (__ "Open sound file" ) [(["*.wav", "*.WAV", "*.Wav"], __ "WAV Sound files")]
  awhen r $ \fp -> do
    let pm = model cenv
    -- pm <- fmap model $ readIORef cenv
    setSoundFilename pm (Just fp)

conditionFilename :: CEnv -> ConditionDirection -> IO()
conditionFilename cenv cd = onViewAsync $ do
  -- (vw, pm) <- fmap (view &&& model) $ readIORef cenv
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
