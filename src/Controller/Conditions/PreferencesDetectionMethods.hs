-- | Keeps the slouching and hunching detection options in sync between model and view
module Controller.Conditions.PreferencesDetectionMethods where

import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import Model.ProtectedModel
import View.MainWindow.Objects

installHandlers :: CRef -> IO()
installHandlers cref = do

  -- Gets the fields from the view
  methodSlouching <- crefReactiveToggleButton preferencesNotebookMethodsSlouchingChkBtn cref
  methodHunching  <- crefReactiveToggleButton preferencesNotebookMethodsHunchingChkBtn cref

  -- Install the conditions
  -- TODO: Uses old Hails API
  installConditions cref
    [ methodSlouching =:= detectionSlouchingEnabledField
    , methodHunching  =:= detectionHunchingEnabledField
    ]
