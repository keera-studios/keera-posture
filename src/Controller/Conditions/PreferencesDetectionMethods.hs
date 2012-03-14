module Controller.Conditions.PreferencesDetectionMethods where

import CombinedEnvironment
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View.MainWindow.Objects

installHandlers :: CRef -> IO()
installHandlers cref = do

  methodSlouching <- crefReactiveToggleButton preferencesNotebookMethodsSlouchingChkBtn cref
  methodHunching  <- crefReactiveToggleButton preferencesNotebookMethodsHunchingChkBtn cref

  installConditions cref
    [ methodSlouching =:= detectionSlouchingEnabledField
    , methodHunching  =:= detectionHunchingEnabledField
    ]
