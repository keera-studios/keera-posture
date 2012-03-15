module Controller.Conditions.PreferencesDetectionCamera where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  cameraEntry <- cenvReactiveSpinButton preferencesNotebookSourceSpin cenv
  installCondition cenv $
    cameraEntry =:= cameraField
