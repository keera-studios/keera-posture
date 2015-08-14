-- Toggles the label of the enable/disable menu item depending on the
-- notification status
module Controller.Conditions.MenuEnabled where

import Control.Monad
import Control.Monad.IfElse
import Graphics.UI.Gtk hiding (menuItemGetLabel)

import Graphics.UI.Gtk.Helpers.MenuItem
import Hails.I18N.Gettext
import CombinedEnvironment
import Hails.MVC.Model.ProtectedModel.Reactive
import I18N.Strings

-- | Update when notification changes and when the program starts
installHandlers :: CEnv -> IO()
installHandlers cenv = onEvents (model cenv) evs (condition cenv)
  where evs = Initialised : events notificationEnabledField

-- | Determine the value that the label should have and update it if necessary
-- TODO: Update to reactive interface using =:= and liftRW
condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  -- notificationEnabledField
  -- =:> modRW vShould mainMenuItemLabel

  menu <- mainMenuEnableItem $ mainWindowBuilder $ view cenv
  awhenM (menuItemGetLabel menu) $ \lbl -> do
    -- Current view value
    mShould <- labelGetText lbl
    
    -- Current model value
    notifyEnabled <- getter notificationEnabledField (model cenv)
    let vShould = if notifyEnabled then strDisable else strEnable
    
    -- Update (M => V ) when necessary
    when (mShould /= vShould) $ labelSetText lbl vShould
