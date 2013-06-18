-- | Keeps the notification delay in sync between model and view
module Controller.Conditions.PreferencesDetectionDelay where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  -- View
  delayEntry <- fmap spinButtonActiveReactive $
                       preferencesNotebookDelaySpinBtn $
                         mainWindowBuilder $ view cenv

  -- Model
  let notificationDelayField' = mkFieldAccessor notificationDelayField (model cenv)

  -- Condition
  delayEntry =:= notificationDelayField'
