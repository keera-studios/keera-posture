-- | Launches the precalibration assistant when requested
module Controller.Conditions.Calibrate where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment

-- | Attaches the condition to button clicks and menu item activations
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 let ui = mainWindowBuilder $ view cenv

 let precalibrateRV = wrapDo_ (precalibrate cenv)
 -- Full RV-based implementation.
 -- precalibrationWindowVisibleRV <- 
 --    windowVisibilityPassive <$> 
 --      precalibrationWindow (mainWindowBuilder $ view cenv)
 -- precalibrateRV = (\() -> True) `liftW` precalibrationWindowVisibleRV

 menu <- mainMenuCalibrateItem ui
 menuItemActivateField menu =:> precalibrateRV

 btn  <- preferencesNotebookCalibrateBtn ui
 buttonActivateField btn =:> precalibrateRV
 -- btn `on` buttonActivated   $ precalibrate cenv

-- | Shows the precalibration window
precalibrate :: CEnv -> IO()
precalibrate cenv = onViewAsync $
 widgetShowAll =<< precalibrationWindow (mainWindowBuilder $ view cenv)
