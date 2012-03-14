-- | Shows the popup menu when the user right-clicks the icon
module Controller.Conditions.Calibrate where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import Paths
import View
import View.MainWindow.Objects
-- import Controller.Conditions.Detector

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 let ui = mainWindowBuilder $ view cenv

 menu <- mainMenuCalibrateItem ui
 menu `on` menuItemActivate $ precalibrate cenv -- onViewAsync $ calibrate cref

 btn  <- preferencesNotebookCalibrateBtn ui
 btn `on` buttonActivated   $ precalibrate cenv -- onViewAsync $ calibrate cref

precalibrate :: CEnv -> IO()
precalibrate cenv = onViewAsync $ do
 let ui = mainWindowBuilder $ view cenv

 calImg <- precalibrationImage ui
 fn     <- getDataFileName "precalibration-preview.png"
 imageSetFromFile calImg fn

 preWin <- precalibrationWindow ui

 widgetShowAll preWin
