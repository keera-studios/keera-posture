module Controller.Conditions.PreferencesUplink where

import CombinedEnvironment
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do

  uplinkUpdates <- fmap toggleButtonActiveReactive $ preferencesNotebookUplinkUpdatesChk $ mainWindowBuilder $ view cenv
  -- uplinkReports <- cenvReactiveToggleButton preferencesNotebookUplinkReportsChk cenv
  let checkUpdatesField' = mkFieldAccessor checkUpdatesField (model cenv)

  uplinkUpdates =:= checkUpdatesField'
    -- , uplinkReports =:= sendReportsField
