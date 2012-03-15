module Controller.Conditions.PreferencesUplink where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do

  uplinkUpdates <- cenvReactiveToggleButton preferencesNotebookUplinkUpdatesChk cenv
  -- uplinkReports <- cenvReactiveToggleButton preferencesNotebookUplinkReportsChk cenv

  installConditions cenv
    [ uplinkUpdates =:= checkUpdatesField 
    -- , uplinkReports =:= sendReportsField
    ]
