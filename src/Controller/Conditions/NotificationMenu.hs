module Controller.Conditions.NotificationMenu where

import CombinedEnvironment
import Graphics.UI.Simplify.Reactive
import Graphics.UI.Gtk.Reactive
import Model.ProtectedModel
import View.MainWindow.Objects

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
