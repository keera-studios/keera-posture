-- | Controls the behaviour of the precalibration step in the calibration
-- assistant
module Controller.Conditions.Precalibration where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import Controller.Conditions.Calibration
import Paths
import I18N.Strings

-- | Detects when the window is closed or its button is depressed
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let ui = mainWindowBuilder $ view cenv
 
  preWin <- precalibrationWindow ui
  preWin `on` deleteEvent $ liftIO (condition cenv) >> return True
 
  btn <- precalibrationOkBtn ui
  btn `on` buttonActivated $ condition cenv

-- | Hides the dialog when necessary, and shows the next step
condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let ui = mainWindowBuilder $ view cenv

  -- Hide precalibration window
  preWin <- precalibrationWindow ui
  widgetHide preWin

  -- Show calibration window
  calWin <- calibrationWindow ui
  widgetShowAll calWin

  -- Disable buttons
  btn1 <- calibrationCloseBtn ui
  btn2 <- calibrationRecalibrateBtn ui

  widgetSetSensitive btn1 False
  widgetSetSensitive btn2 False

  -- Readjust title, explanation and image
  lblT <- calibrationTitleLbl ui
  labelSetMarkup lblT title1

  lblE <- calibrationExplanationLbl ui
  labelSetText lblE explanation1
  
  calImg <- calibrationImage ui
  fn     <- getDataFileName "calibration-preview.png"
  imageSetFromFile calImg fn

  -- Run calibration
  calibrate cenv
