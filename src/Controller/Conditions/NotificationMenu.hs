module Controller.Conditions.NotificationMenu where

import Data.ReactiveValue
import Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do

  notifyIcon <- fmap checkMenuItemActiveReactive $ mainMenuNotifyIconItem $ mainWindowBuilder $ view cenv
  -- notifyBubble  <- cenvReactiveCheckMenuItem mainMenuNotifyBubbleItem  cenv
  notifySound   <- fmap checkMenuItemActiveReactive $ mainMenuNotifySoundItem   $ mainWindowBuilder $ view cenv
  notifyOverlay <- fmap checkMenuItemActiveReactive $ mainMenuNotifyOverlayItem $ mainWindowBuilder $ view cenv

  -- installConditions cenv
  notifyIcon    =:= mkFieldAccessor notificationIconEnabledField (model cenv)
  -- notifyBubble  =:= notificationBubbleEnabledField (model cenv)
  notifySound   =:= mkFieldAccessor notificationSoundEnabledField (model cenv)
  notifyOverlay =:= mkFieldAccessor notificationOverlayEnabledField (model cenv)
