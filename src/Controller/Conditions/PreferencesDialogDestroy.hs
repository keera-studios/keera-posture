-- | Hides the preferences dialog when the user closes it
module Controller.Conditions.PreferencesDialogDestroy where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
 dg <- preferencesDialog $ mainWindowBuilder $ view cenv
 windowCloseReactive dg =:> constW False (widgetVisibleReactive dg)

-- import Control.Monad
-- import Control.Monad.Reader (liftIO)
-- import Graphics.UI.Gtk
 --  dg `on` deleteEvent $ liftIO (onViewAsync (widgetHide dg)) >> return True
