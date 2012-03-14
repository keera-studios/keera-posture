{-# LANGUAGE CPP #-}
-- | Shows the popup menu when the user right-clicks the icon
module Controller.Conditions.PopupMenu where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import View
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  icon `on` statusIconPopupMenu $ condition cenv
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  -- menu `onFocusOut` const (liftIO $ onViewAsync (myPutStrLn "Going down" >> menuPopdown menu) >> return False)
  menu `onFocusOut` const (liftIO $ onViewAsync (menuPopdown menu) >> return False)

  menuClose <- mainMenuClose $ mainWindowBuilder $ view cenv
  menuSep   <- mainMenuSeparator0 $ mainWindowBuilder $ view cenv
#ifdef linux_HOST_OS
  widgetHide menuClose
  widgetHide menuSep
#else
  menuClose `on` menuItemActivate $ onViewAsync (menuPopdown menu)
  widgetShow menuClose
  widgetShow menuSep
#endif

condition :: CEnv -> Maybe MouseButton -> TimeStamp -> IO()
condition cenv _m _t = onViewAsync $ do
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  -- This should be some other way

  menuPopup menu Nothing
