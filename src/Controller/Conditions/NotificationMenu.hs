-- | Keep the four notification mechanisms in sync between the model and the view
module Controller.Conditions.NotificationMenu where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment

-- | Establishes the synchronization conditions between model and view for
-- selecting which notification methods are enabled.
installHandlers :: CEnv -> IO()
installHandlers cenv = do

  -- Get elements from the view
  notifyIcon <- fmap checkMenuItemActiveReactive $ mainMenuNotifyIconItem $ mainWindowBuilder $ view cenv
  -- notifyBubble  <- cenvReactiveCheckMenuItem mainMenuNotifyBubbleItem  cenv
  notifySound   <- fmap checkMenuItemActiveReactive $ mainMenuNotifySoundItem   $ mainWindowBuilder $ view cenv
  notifyOverlay <- fmap checkMenuItemActiveReactive $ mainMenuNotifyOverlayItem $ mainWindowBuilder $ view cenv

  -- Install bidirectional conditions
  notifyIcon    =:= mkFieldAccessor notificationIconEnabledField (model cenv)
  -- notifyBubble  =:= notificationBubbleEnabledField (model cenv)
  notifySound   =:= mkFieldAccessor notificationSoundEnabledField (model cenv)
  notifyOverlay =:= mkFieldAccessor notificationOverlayEnabledField (model cenv)
