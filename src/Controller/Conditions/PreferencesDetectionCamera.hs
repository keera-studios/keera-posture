module Controller.Conditions.PreferencesDetectionCamera where

import CombinedEnvironment
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  cameraEntry <- fmap spinButtonActiveReactive $ preferencesNotebookSourceSpin $ mainWindowBuilder $ view cenv
  let cameraField' = mkFieldAccessor cameraField (model cenv)
  cameraEntry =:= cameraField'
