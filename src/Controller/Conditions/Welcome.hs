-- | Shows a splash window giving the user some basic instructions. This will
-- only be shown if the user executes the program for the first time.
module Controller.Conditions.Welcome where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  -- View elements
  let ui = mainWindowBuilder $ view cenv
  win <- welcomeWindow ui
  btn <- welcomeOkBtn ui

  -- Model RVs
  let firstRun = mkFieldAccessor firstRunField (model cenv)

  -- Show the welcome window only when necessary (if this is not the first time
  -- that the program is executed)
  guardRO ((Just True ==) `liftR` firstRun) =:> widgetVisibleReactive win
     
  -- Hide the window when requested
  windowCloseReactive win =:> constW False (widgetVisibleReactive win)
  buttonActivateField btn =:> constW False (widgetVisibleReactive win)

-- import Control.Arrow
-- import Control.Monad.Trans
-- import Graphics.UI.Gtk
--
-- let (ui, pm) = ((mainWindowBuilder . view) &&& model) cenv
-- onEvent pm FirstRunChanged $ condition cenv
-- win `on` deleteEvent $ liftIO (onViewAsync (widgetHide win)) >> return True
-- btn `on` buttonActivated $ onViewAsync (widgetHide win)
-- -- Shows the window if this is the first time the program is run
-- condition :: CEnv -> IO()
-- condition cenv = onViewAsync $ do
--   fr <- getter firstRunField $ model cenv
--   when (fr == Just True) $ 
--     widgetShowAll =<< welcomeWindow (mainWindowBuilder $ view cenv)
