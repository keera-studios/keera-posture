-- | Runs the preferences dialog when the user requests it, hides it once it's
-- closed.
module Controller.Conditions.PreferencesDialog where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 menu <- mainMenuPreferences $ mainWindowBuilder $ view cenv
 menu `on` menuItemActivate $ condition cenv

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
 dg <- preferencesDialog $ mainWindowBuilder $ view cenv
 _  <- dialogRun dg
 widgetHide dg
