module Controller.Conditions.UpdateCheck where

-- External libraries
import qualified Hails.Graphics.UI.Gtk.Simplify.UpdateCheck as UpdateCheck

-- Internal libraries
import CombinedEnvironment
import Model.ProtectedModel

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  -- FIXME: do this only if the user has enabled it
  let pm = model cenv
  onEvent pm Initialised $ onViewAsync $ UpdateCheck.condition cenv
