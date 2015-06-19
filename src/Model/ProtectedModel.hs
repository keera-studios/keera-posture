module Model.ProtectedModel
   ( ProtectedModel
   , onEvent
   , onEvents
   , waitFor
   , module Exported
   )
  where

import Hails.MVC.Model.ProtectedModel.Initialisation as Exported
import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents     as Exported
import Model.ProtectedModel.Preferences    as Exported
import Model.ProtectedModel.Status         as Exported
import Model.ProtectedModel.Sound          as Exported
import Model.ProtectedModel.Detector       as Exported

onEvents pm es x = mapM_ (\e -> onEvent pm e x) es
