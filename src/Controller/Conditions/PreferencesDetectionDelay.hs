module Controller.Conditions.PreferencesDetectionDelay where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  delayEntry <- cenvReactiveSpinButton preferencesNotebookDelaySpinBtn cenv
  installCondition cenv $
    delayEntry =:= notificationDelayField
