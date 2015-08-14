-- | Manages the interaction with one of the calibration steps (calibration
-- results).
module Controller.Conditions.Postcalibration where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import Controller.Conditions.Calibration

-- | Detects when the dialog is closed, or when its buttons are pressed.
installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let ui = mainWindowBuilder $ view cenv
 
  calWin <- calibrationWindow ui
  btnCls <- calibrationCloseBtn ui
  btnCal <- calibrationRecalibrateBtn ui

  let mustClose = lMerge (windowCloseReactive calWin) 
                         (buttonActivateField btnCls)

      mustReallyClose = mustClose `governingR` btnClsSensitive

      btnClsSensitive = widgetSensitiveReactive btnCls
      btnCalSensitive = widgetSensitiveReactive btnCal

  mustReallyClose            =:> widgetVisibleReactive calWin
  buttonActivateField btnCal =:> (constW False btnClsSensitive)
  buttonActivateField btnCal =:> (constW False btnCalSensitive)
  buttonActivateField btnCal =:> wrapDo_ (calibrate cenv)
    

  -- import Control.Monad
  -- import Control.Monad.Trans
  -- import Graphics.UI.Gtk
  -- calWin `on` deleteEvent $ liftIO (handleClose cenv)
 
  -- btnCls <- calibrationCloseBtn ui
  -- btnCls `on` buttonActivated $ onViewAsync (void (handleClose cenv))
  -- btnCal `on` buttonActivated $ handleRecalibrate cenv
 

-- -- Close the calibration window when appropriate
-- handleClose :: CEnv -> IO Bool
-- handleClose cenv = do
--   -- calCloseSensitive = widgetSensitive btnCls
--   
--   let ui = mainWindowBuilder $ view cenv
--   btn       <- calibrationCloseBtn ui
--   isEnabled <- get btn widgetSensitive 
--   calWin    <- calibrationWindow ui
--   if isEnabled then widgetHide calWin else widgetShowAll calWin
--   return isEnabled

-- Request a recalibration when instructed by the user
-- handleRecalibrate :: CEnv -> IO ()
-- handleRecalibrate cenv = onViewAsync $ do
--   let ui = mainWindowBuilder $ view cenv
--   btn1 <- calibrationCloseBtn ui
--   btn2 <- calibrationRecalibrateBtn ui
-- 
--   widgetSetSensitive btn1 False
--   widgetSetSensitive btn2 False
-- 
--   calibrate cenv
