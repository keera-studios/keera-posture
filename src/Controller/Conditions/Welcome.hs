module Controller.Conditions.Welcome where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import CombinedEnvironment
import Model.ProtectedModel
import Model.ProtectedModel.Reactive
import Paths
import View
import View.MainWindow.Objects


installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let (vw, pm) = (view &&& model) cenv
  win      <- welcomeWindow $ mainWindowBuilder vw
  onEvent pm FirstRunChanged $ condition cenv

  win `on` deleteEvent $ liftIO (onViewAsync (widgetHide win)) >> return True

  btn <- welcomeOkBtn $ mainWindowBuilder vw
  btn `on` buttonActivated $ onViewAsync (widgetHide win)

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv
  win      <- welcomeWindow $ mainWindowBuilder vw
  img      <- welcomeImage $ mainWindowBuilder vw

  fr <- getter firstRunField pm
  when (fr == Just True) $ do
    fn <- getDataFileName "welcome-screen.png"
    imageSetFromFile img fn
    widgetShowAll win
