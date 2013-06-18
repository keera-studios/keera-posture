-- | Quit the program when the user requests it
module Controller.Conditions.Quit where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

-- | Detect when the user activates the menu item 'Quit'
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuQuit $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $ condition cenv

-- | Deinstalls the status icon and stops the view, effectively stopping the
-- whole program.
condition :: CEnv -> IO()
condition cenv = do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  statusIconSetVisible icon False
  mainQuit
