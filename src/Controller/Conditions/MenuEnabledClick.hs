-- | Toggles notification status when the user interacts with the status icon
module Controller.Conditions.MenuEnabledClick where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive

-- Detects interaction with the main window and the tray icon
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuEnableItem $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $ condition cenv
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  icon `on` statusIconActivate $ condition cenv

-- | Closes the menu and toggles notification status
condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  -- Close the menu if the option is selected
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  menuPopdown menu

  let pm = model cenv

  status <- getStatus pm
  -- notifyEnabled <- getter notificationEnabledField pm

  case status of 
   StatusDisabled     -> do setStatus pm StatusIdle
                            setter notificationEnabledField pm True
   StatusCallibrating -> return ()
   _                  -> do setStatus pm StatusDisabled
                            setter notificationEnabledField pm False
