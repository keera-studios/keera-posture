module Controller.Conditions.Postcalibration where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import View
import View.MainWindow.Objects
import Controller.Conditions.Detector

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  -- ui <- fmap (mainWindowBuilder . view) $ readIORef cenv
  let ui = mainWindowBuilder $ view cenv
 
  calWin <- calibrationWindow ui
  calWin `on` deleteEvent $ liftIO (handleClose cenv)
 
  btnCls <- calibrationCloseBtn ui
  btnCls `on` buttonActivated $ onViewAsync (void (handleClose cenv))
 
  btnCal <- calibrationRecalibrateBtn ui
  btnCal `on` buttonActivated $ handleRecalibrate cenv

handleClose :: CEnv -> IO Bool
handleClose cenv = do
  -- ui        <- fmap (mainWindowBuilder . view) $ readIORef cenv
  let ui = mainWindowBuilder $ view cenv
  btn       <- calibrationCloseBtn ui
  isEnabled <- get btn widgetSensitive 
  calWin <- calibrationWindow ui

  if isEnabled
   then do widgetHide calWin
           return True
   else do widgetShowAll calWin
           return False

handleRecalibrate :: CEnv -> IO ()
handleRecalibrate cenv = onViewAsync $ do
  -- ui   <- fmap (mainWindowBuilder . view) $ readIORef cenv
  let ui = mainWindowBuilder $ view cenv
  btn1 <- calibrationCloseBtn ui
  btn2 <- calibrationRecalibrateBtn ui

  widgetSetSensitive btn1 False
  widgetSetSensitive btn2 False

  calibrate cenv
