-- | Keeps the correction factor in sync between model and view
module Controller.Conditions.PreferencesDetectionCorrection where

import Control.Applicative
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  -- View
  correctionEntry <- spinButtonValueIntEditReactive <$>
                            (preferencesNotebookCorrectionSpinBtn $
                              mainWindowBuilder $ view cenv)

  -- Model
  let correctionFactorField' = mkFieldAccessor correctionFactorField (model cenv)

  -- Condition
  correctionEntry =:= correctionFactorField'
