module Controller.Conditions.PreferencesLanguageCombo where

import CombinedEnvironment
import Hails.MVC.Controller.Reactive
import Graphics.UI.Gtk.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let store = languageListStore $ view cenv
  languageCombo <- cenvReactiveTypedComboBoxUnsafe store preferencesNotebookLanguageCombo cenv
  installCondition cenv $
    languageCombo =:= languageField
  
