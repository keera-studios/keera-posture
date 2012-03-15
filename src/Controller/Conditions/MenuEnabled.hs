module Controller.Conditions.MenuEnabled where

import Control.Arrow
import Control.Monad
import Control.Monad.IfElse
import Graphics.UI.Gtk

import CombinedEnvironment
import Gettext
import Graphics.UI.Gtk.Helpers.MenuItem
import Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let pm = model cenv
  onEvent pm Initialised $ condition cenv
  mapM_ (\ev -> onEvent pm ev (condition cenv)) $ events notificationEnabledField

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let (ui, pm) = ((mainWindowBuilder . view) &&& model) cenv

  menu <- mainMenuEnableItem ui
  awhenM (menuItemGetLabel menu) $ \lbl-> do

    -- Current view value
    mShould <- labelGetText lbl
    
    -- Current model value
    notifyEnabled <- getter notificationEnabledField pm
    let vShould = if notifyEnabled then __ "Disable" else __ "Enable"
    
    -- Update (M => V ) when necessary
    when (mShould /= vShould) $ labelSetText lbl vShould
