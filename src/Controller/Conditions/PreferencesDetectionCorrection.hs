module Controller.Conditions.PreferencesDetectionCorrection where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  correctionEntry <- cenvReactiveSpinButton preferencesNotebookCorrectionSpinBtn cenv
  installCondition cenv $
    correctionEntry =:= correctionFactorField
