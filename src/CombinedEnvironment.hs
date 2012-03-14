module CombinedEnvironment
   ( CEnv
   , view
   , GEnv.model
   , GEnv.createCEnv
   -- , GEnv.createCRef
   -- , GEnv.readIORef
   -- , GEnv.writeIORef
   , updateView
   , onViewAsync
   , GEnv.installCondition
   , GEnv.installConditions
   )
  where

-- Internal libraries
import qualified Graphics.UI.Gtk.GtkView as GView
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv

import View
import Model.ReactiveModel.ModelEvents
import Model.Model

type CEnv = GEnv.CEnv View Model ModelEvent
-- type CRef = GEnv.CRef View Model ModelEvent

view :: CEnv -> View
view = GView.getGUI . GEnv.view

updateView :: CEnv -> View -> CEnv
updateView cenv v = cenv { GEnv.view = GView.GtkView v }
