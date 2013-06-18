-- | Manages the interaction with one of the calibration steps (calibration
-- results).
module Controller.Conditions.Postcalibration where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import Controller.Conditions.Calibration

-- | Detects when the dialog is closed, or when its buttons are pressed.
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let ui = mainWindowBuilder $ view cenv
 
  calWin <- calibrationWindow ui
  calWin `on` deleteEvent $ liftIO (handleClose cenv)
 
  btnCls <- calibrationCloseBtn ui
  btnCls `on` buttonActivated $ onViewAsync (void (handleClose cenv))
 
  btnCal <- calibrationRecalibrateBtn ui
  btnCal `on` buttonActivated $ handleRecalibrate cenv

-- Close the calibration window when appropriate
handleClose :: CEnv -> IO Bool
handleClose cenv = do
  let ui = mainWindowBuilder $ view cenv
  btn       <- calibrationCloseBtn ui
  isEnabled <- get btn widgetSensitive 
  calWin    <- calibrationWindow ui
  if isEnabled then widgetHide calWin else widgetShowAll calWin
  return isEnabled

-- Request a recalibration when instructed by the user
handleRecalibrate :: CEnv -> IO ()
handleRecalibrate cenv = onViewAsync $ do
  let ui = mainWindowBuilder $ view cenv
  btn1 <- calibrationCloseBtn ui
  btn2 <- calibrationRecalibrateBtn ui

  widgetSetSensitive btn1 False
  widgetSetSensitive btn2 False

  calibrate cenv
