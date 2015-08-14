-- | Runs the preferences dialog when the user requests it, hides it once it's
-- closed.
module Controller.Conditions.PreferencesDialog where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
 menu <- mainMenuPreferences $ mainWindowBuilder $ view cenv
 menuItemActivateField menu =:> wrapDo_ (condition cenv)

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
 dg <- preferencesDialog $ mainWindowBuilder $ view cenv
 _  <- dialogRun dg
 widgetHide dg
