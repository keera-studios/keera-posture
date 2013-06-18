-- | Hides the preferences dialog when the user closes it
module Controller.Conditions.PreferencesDialogDestroy where

import Control.Monad
import Control.Monad.Reader (liftIO)
import Graphics.UI.Gtk

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
 dg <- preferencesDialog $ mainWindowBuilder $ view cenv
 dg `on` deleteEvent $ liftIO (onViewAsync (widgetHide dg)) >> return True
