-- Toggles the label of the enable/disable menu item depending on whether the
-- system is enabled or disabled
module Controller.Conditions.MenuEnabled where

import Control.Monad
import Control.Monad.IfElse
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.MenuItem

import CombinedEnvironment
import Hails.MVC.Model.ProtectedModel.Reactive
import I18N.Strings

installHandlers :: CEnv -> IO()
installHandlers cenv = onEvents (model cenv) evs (condition cenv)
  where evs = Initialised : events notificationEnabledField

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  menu <- mainMenuEnableItem $ mainWindowBuilder $ view cenv
  awhenM (menuItemGetLabel menu) $ \lbl -> do
    -- Current view value
    mShould <- labelGetText lbl
    
    -- Current model value
    notifyEnabled <- getter notificationEnabledField (model cenv)
    let vShould = if notifyEnabled then strDisable else strEnable
    
    -- Update (M => V ) when necessary
    when (mShould /= vShould) $ labelSetText lbl vShould
