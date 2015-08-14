-- | Opens the default browser with the project's URL when the users activates
-- the help menuitem.
module Controller.Conditions.Help where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import System.Application

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  menu <- mainMenuHelp $ mainWindowBuilder $ view cenv
  menuItemActivateField menu =:> wrapDo_ 
    (onViewAsync $ void $
      openUrlBySystemTool "http://www.keera.co.uk/projects/keera-posture/documentation/")
