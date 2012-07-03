module Controller.Conditions.PreferencesDetectionDelay where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  delayEntry <- fmap spinButtonActiveReactive $ preferencesNotebookDelaySpinBtn $ mainWindowBuilder $ view cenv
  let notificationDelayField' = mkFieldAccessor notificationDelayField (model cenv)

  delayEntry =:= notificationDelayField'
