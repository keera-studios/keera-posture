module Controller.Conditions.PreferencesDetectionCorrection where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  correctionEntry <- cenvReactiveSpinButton preferencesNotebookCorrectionSpinBtn cenv
  installCondition cenv $
    correctionEntry =:= correctionFactorField
