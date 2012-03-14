module Controller.Conditions.NotifySound where

import Control.Concurrent
import Control.Monad

import CombinedEnvironment
import Model.Model (Status(..))
import Model.ProtectedModel
import Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let pm = model cenv
  onEvent pm Initialised         $ condition cenv
  onEvent pm NotificationToggled $ condition cenv
  onEvent pm StatusChanged       $ condition cenv
  void $ forkIO $ startAudio pm

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let pm = model cenv
  status        <- getStatus pm
  notifyEnabled <- getter notificationEnabledField pm
  soundEnabled  <- getter notificationSoundEnabledField pm
  
  if status == StatusNotifying && notifyEnabled && soundEnabled
   then startPlaying pm
   else stopPlaying pm
