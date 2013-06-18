-- | Opens the default browser with the project's URL when the users activates
-- the help menuitem.
module Controller.Conditions.Help where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import System.Application

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuHelp $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $ onViewAsync $ void $
    openUrlBySystemTool "http://www.keera.es/projects/keera-posture/documentation/"
