-- | Opens the default browser with the project URL
module Controller.Conditions.Website where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import System.Application

-- | Opens the project's url when the user requests it
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuWebsite $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $
    onViewAsync $ void $ openUrlBySystemTool "http://www.keera.es/projects/keera-posture/"
