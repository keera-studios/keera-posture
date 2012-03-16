module Controller.Conditions.NotificationMenu where

import Hails.MVC.Controller.Reactive
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = do

  notifyIcon    <- cenvReactiveCheckMenuItem mainMenuNotifyIconItem    cenv
  -- notifyBubble  <- cenvReactiveCheckMenuItem mainMenuNotifyBubbleItem  cenv
  notifySound   <- cenvReactiveCheckMenuItem mainMenuNotifySoundItem   cenv
  notifyOverlay <- cenvReactiveCheckMenuItem mainMenuNotifyOverlayItem cenv

  installConditions cenv
    [ notifyIcon    =:= notificationIconEnabledField
    -- , notifyBubble  =:= notificationBubbleEnabledField
    , notifySound   =:= notificationSoundEnabledField
    , notifyOverlay =:= notificationOverlayEnabledField
    ]
