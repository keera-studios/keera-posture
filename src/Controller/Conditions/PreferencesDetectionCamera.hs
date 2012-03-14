module Controller.Conditions.PreferencesDetectionCamera where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  cameraEntry <- cenvReactiveSpinButton preferencesNotebookSourceSpin cenv
  installCondition cenv $
    cameraEntry =:= cameraField
