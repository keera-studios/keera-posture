-- | Opens the default browser with the project URL
module Controller.Conditions.Website where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import System.Application

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuWebsite $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $
    onViewAsync $ openUrlBySystemTool "http://www.keera.es/projects/keera-posture/"
