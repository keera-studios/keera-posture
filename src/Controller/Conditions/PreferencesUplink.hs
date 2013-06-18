-- | Keeps the 'check updates' preference option in sync between
-- model and view
module Controller.Conditions.PreferencesUplink where

import CombinedEnvironment
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do

  -- View
  uplinkUpdates <- fmap toggleButtonActiveReactive $ preferencesNotebookUplinkUpdatesChk $ mainWindowBuilder $ view cenv
  -- uplinkReports <- cenvReactiveToggleButton preferencesNotebookUplinkReportsChk cenv
  
  -- Model
  let checkUpdatesField' = mkFieldAccessor checkUpdatesField (model cenv)

  -- Condition
  uplinkUpdates =:= checkUpdatesField'
  -- , uplinkReports =:= sendReportsField
