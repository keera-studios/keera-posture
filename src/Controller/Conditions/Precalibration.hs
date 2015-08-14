-- | Controls the behaviour of the precalibration step in the calibration
-- assistant
module Controller.Conditions.Precalibration where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import Controller.Conditions.Calibration
import Paths
import I18N.Strings

-- | Detects when the window is closed or its button is depressed
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let ui = mainWindowBuilder $ view cenv

  -- Precalibration widgets
  preWin <- precalibrationWindow ui
  btn    <- precalibrationOkBtn ui

  -- Calibration widgets
  calWin <- calibrationWindow ui

  -- Signal that triggers changes
  let precalibrationNext = lMerge (windowCloseReactive preWin)
                                  (buttonActivateField btn) 

  -- Hide the precalibration window
  precalibrationNext =:> constW False (widgetVisibleReactive preWin)

  -- Show the calibration window
  precalibrationNext =:> constW True (widgetVisibleReactive calWin)

  btnCls <- calibrationCloseBtn ui
  btnCal <- calibrationRecalibrateBtn ui
  let btnClsSensitive = widgetSensitiveReactive btnCls
      btnCalSensitive = widgetSensitiveReactive btnCal

  lblT <- calibrationTitleLbl ui
  lblE <- calibrationExplanationLbl ui

  calImg <- calibrationImage ui
  fn     <- getDataFileName "calibration-preview.png"

  precalibrationNext =:> 
      (   (constW False        (btnClsSensitive &&& btnCalSensitive))
      &&& (constW title1       (labelTextReactive lblT))
      &&& (constW explanation1 (labelTextReactive lblE))
      &&& (constW fn           (imageFileReactive calImg))
      &&& (wrapDo_             (calibrate cenv))
      )
 
-- import Graphics.UI.Gtk
---- | Hides the dialog when necessary, and shows the next step
--condition :: CEnv -> IO()
--condition cenv = onViewAsync $ do
--  let ui = mainWindowBuilder $ view cenv
--
--  -- -- Hide precalibration window
--  -- preWin <- precalibrationWindow ui
--  -- widgetHide preWin
--
--  -- -- Show calibration window
--  -- calWin <- calibrationWindow ui
--  -- widgetShowAll calWin
--
--  -- -- Disable buttons
--  -- btn1 <- calibrationCloseBtn ui
--  -- btn2 <- calibrationRecalibrateBtn ui
--
--  -- widgetSetSensitive btn1 False
--  -- widgetSetSensitive btn2 False
--
--  -- Readjust title, explanation and image
--  lblT <- calibrationTitleLbl ui
--  reactiveValueWrite (labelTextReactive lblT) title1
--
--  lblE <- calibrationExplanationLbl ui
--  reactiveValueWrite (labelTextReactive lblE) title1
--  
--  calImg <- calibrationImage ui
--  fn     <- getDataFileName "calibration-preview.png"
--  reactiveValueWrite (imageFileReactive calImg) fn
--
--  -- Run calibration
--  calibrate cenv
--
