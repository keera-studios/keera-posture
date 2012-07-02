-- | Launches the precalibration assistant when requested
module Controller.Conditions.Calibrate where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 let ui = mainWindowBuilder $ view cenv

 menu <- mainMenuCalibrateItem ui
 menu `on` menuItemActivate $ precalibrate cenv

 btn  <- preferencesNotebookCalibrateBtn ui
 btn `on` buttonActivated   $ precalibrate cenv

precalibrate :: CEnv -> IO()
precalibrate cenv = onViewAsync $
 widgetShowAll =<< precalibrationWindow (mainWindowBuilder $ view cenv)
