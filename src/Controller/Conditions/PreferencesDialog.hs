-- | Shows the popup menu when the user right-clicks the icon
module Controller.Conditions.PreferencesDialog where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import View
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 menu <- mainMenuPreferences $ mainWindowBuilder $ view cenv
 menu `on` menuItemActivate $ condition cenv

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
 dg <- preferencesDialog $ mainWindowBuilder $ view cenv
 _  <- dialogRun dg
 widgetHide dg
