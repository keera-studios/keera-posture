module Controller.Conditions.Welcome where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let (ui, pm) = ((mainWindowBuilder . view) &&& model) cenv

  -- Show the welcome window only when necessary
  onEvent pm FirstRunChanged $ condition cenv

  -- Hide the window when requested
  win <- welcomeWindow ui
  win `on` deleteEvent $ liftIO (onViewAsync (widgetHide win)) >> return True
  btn <- welcomeOkBtn ui
  btn `on` buttonActivated $ onViewAsync (widgetHide win)

-- Shows the window if this is the first time the program is run
condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  fr <- getter firstRunField $ model cenv
  when (fr == Just True) $ 
    widgetShowAll =<< (welcomeWindow $ mainWindowBuilder $ view cenv)
