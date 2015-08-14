-- | Opens the default browser with the project URL
module Controller.Conditions.Website where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import System.Application

-- | Opens the project's url when the user requests it
installHandlers :: CEnv -> IO()
installHandlers cenv = do
  menu <- mainMenuWebsite $ mainWindowBuilder $ view cenv
  menuItemActivateField menu =:> 
    wrapDo_ (onViewAsync $ void $ openUrlBySystemTool "http://www.keera.es/projects/keera-posture/")
