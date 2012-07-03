module Controller.Conditions.PreferencesDetectionCorrection where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  correctionEntry <- fmap spinButtonActiveReactive $ preferencesNotebookCorrectionSpinBtn $ mainWindowBuilder $ view cenv
  let correctionFactorField' = mkFieldAccessor correctionFactorField (model cenv)

  correctionEntry =:= correctionFactorField'
