-- | Keeps the camera selection in sync between model and view
module Controller.Conditions.PreferencesDetectionCamera where

import CombinedEnvironment
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  -- View
  cameraEntry <- fmap spinButtonActiveReactive $
                        preferencesNotebookSourceSpin $ mainWindowBuilder $
                          view cenv
  -- Model
  let cameraField' = mkFieldAccessor cameraField (model cenv)

  -- Condition
  cameraEntry =:= cameraField'
