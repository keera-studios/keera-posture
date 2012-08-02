-- | Notify using sound when that notification method is enabled and the user
-- is not sitting properly
module Controller.Conditions.NotifySound where

import Control.Concurrent
import Control.Monad

import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive

-- | Install event handlers and start audio subsystem
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let pm = model cenv
  onEvent pm Initialised                $ condition cenv
  onEvent pm NotificationEnabledChanged $ condition cenv
  onEvent pm StatusChanged              $ condition cenv
  void $ forkIO $ startAudio pm

-- | Notify/stop notifying
condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let pm = model cenv
  status        <- getStatus pm
  notifyEnabled <- getter notificationEnabledField pm
  soundEnabled  <- getter notificationSoundEnabledField pm
  
  if status == StatusNotifying && notifyEnabled && soundEnabled
   then startPlaying pm
   else stopPlaying pm
