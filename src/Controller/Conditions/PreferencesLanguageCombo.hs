module Controller.Conditions.PreferencesLanguageCombo where

import CombinedEnvironment
import Graphics.UI.Simplify.Reactive
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View
import View.MainWindow.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let store = languageListStore $ view cenv
  languageCombo <- cenvReactiveTypedComboBoxUnsafe store preferencesNotebookLanguageCombo cenv
  installCondition cenv $
    languageCombo =:= languageField
  
