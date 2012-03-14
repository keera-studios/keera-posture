-- | Opens the default browser with the project URL
module Controller.Conditions.Help where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import System.Application
import View
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuHelp $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $ onViewAsync $
    openUrlBySystemTool "http://www.keera.es/projects/keera-posture/documentation/"
