module Controller.Conditions.PreferencesDetectionDelay where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  delayEntry <- cenvReactiveSpinButton preferencesNotebookDelaySpinBtn cenv
  installCondition cenv $
    delayEntry =:= notificationDelayField
