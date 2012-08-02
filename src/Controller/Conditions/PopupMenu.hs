{-# LANGUAGE CPP #-}
-- | Shows the popup menu when the user right-clicks the icon. The exact
-- behaviour depends a little on the OS, but the overall effect is more or less
-- similar.
module Controller.Conditions.PopupMenu where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment

-- | Shows/hides the menu as instructed by the user
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  icon `on` statusIconPopupMenu $ condition cenv
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  menu `onFocusOut` const (liftIO $ onViewAsync (menuPopdown menu) >> return False)
#ifndef linux_HOST_OS
  menuClose <- mainMenuClose $ mainWindowBuilder $ view cenv
  menuClose `on` menuItemActivate $ onViewAsync (menuPopdown menu)
#endif

-- | Adapts the menu to the OS and shows it
condition :: CEnv -> Maybe MouseButton -> TimeStamp -> IO()
condition cenv _m _t = onViewAsync $ do
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  -- This should be some other way
  widgetShowAll menu

  menuClose <- mainMenuClose $ mainWindowBuilder $ view cenv
  menuSep   <- mainMenuSeparator0 $ mainWindowBuilder $ view cenv
#ifdef linux_HOST_OS
  widgetHide menuClose
  widgetHide menuSep
#else
  widgetShow menuClose
  widgetShow menuSep
#endif
  menuPopup menu Nothing
