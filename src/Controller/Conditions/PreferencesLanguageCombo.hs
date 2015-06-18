-- | Keeps the currently, selected language in sync between model and view
module Controller.Conditions.PreferencesLanguageCombo where

-- External imports
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  return ()
  -- let store = languageListStore $ view cenv
  -- combo <- preferencesNotebookLanguageCombo (mainWindowBuilder (view cenv))
  -- let languageComboR = typedComboBoxUnsafeReactive store combo
  --     languageField' = mkFieldAccessor languageField $ model cenv
  --     
  -- -- installCondition cenv $
  -- languageComboR =:= languageField'
