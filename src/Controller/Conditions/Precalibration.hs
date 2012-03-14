-- | Shows the popup menu when the user right-clicks the icon
module Controller.Conditions.Precalibration where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import Gettext
import View
import View.MainWindow.Objects
import Paths
import Controller.Conditions.Detector

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  -- ui <- fmap (mainWindowBuilder . view) $ readIORef cenv
  let ui = mainWindowBuilder $ view cenv
 
  preWin <- precalibrationWindow ui
  preWin `on` deleteEvent $ liftIO (condition cenv) >> return True
 
  btn <- precalibrationOkBtn ui
  btn `on` buttonActivated $ condition cenv

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  -- ui <- fmap (mainWindowBuilder . view) $ readIORef cenv
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

explanation1 :: String
explanation1 = __ "This is the calibration assistant.\n" ++
               __ "Keera Posture will look at you now."

title1 :: String
title1 = __ "<b>Calibration</b>"
