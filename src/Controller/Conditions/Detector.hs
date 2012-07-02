module Controller.Conditions.Detector where

-- External imports
import           AI.CV.ImageProcessors
import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Exception.Extra
import           Control.Monad
import           Control.Monad.IfElse
import           Data.Maybe

-- Internal imports
import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive
import AI.CV.PostureProcessors

installHandlers :: CEnv -> IO()
installHandlers cenv = do
 let pm = model cenv
 onEvents pm [ Initialised, NotificationEnabledChanged ] $ startDetector cenv
 onEvent pm CameraChanged $ restartDetector cenv

-- | Runs the detector with the configured parameters until it is disabled
startDetector :: CEnv -> IO ()
startDetector cenv = void $ forkOS $ whenM (detectorCanStart cenv) $ do
  let pm = model cenv

  -- Mark that the detector is running in the model
  setDetector pm True

  cal <- getter calibrationParamsField pm
  del <- getter notificationDelayField pm
  cor <- getter correctionFactorField  pm
  cam <- getter cameraField            pm

  when (isJust cal) $ do
    state <- createInitialState cor del (fromJust cal)
    E.handle (anyway $ wrongCam pm) $ do
      detectorProc <- initialiseDetector cam state
      setter cameraStatusField pm (Just True)
      void $ detectorProc `runTill` updateStatus cenv

  -- Mark the detector as not running in the model
  setDetector pm False

restartDetector :: CEnv -> IO()
restartDetector cenv = void $ forkIO $ do
  let pm = model cenv

  -- Remember if the system was enabled and restore the same state later.
  enabled <- getter notificationEnabledField pm
  setter notificationEnabledField pm False

  -- This value is completely random. It simply works, but there must be a
  -- better way.
  threadDelay 1000000

  -- What for the detector to die
  waitForDetector pm

  -- Reset the notification status
  setter notificationEnabledField pm enabled

  -- Start the system again
  startDetector cenv

detectorCanStart :: CEnv -> IO Bool
detectorCanStart cenv = do
  let pm = model cenv
  -- Basic conf from file is ok
  confOk  <- fmap isJust $ getter calibrationParamsField (model cenv)
  -- Detection is enabled
  st      <- getter statusField pm
  notif   <- getter notificationEnabledField pm
  let detectionEnabled = notif && st < StatusCallibrating
  -- No detector is running
  detectorNotRunning <- fmap not $ getDetector pm
  return (confOk && detectionEnabled && detectorNotRunning)

updateStatus :: CEnv -> InternalState -> IO Bool
updateStatus cenv state = do
  let pm = model cenv
  notif <- getter notificationEnabledField pm
  st    <- getter statusField pm
  let detectionEnabled = notif && st < StatusCallibrating

  -- Update the status when detection is enabled
  when detectionEnabled $ setter statusField pm (newStatus (detectionStatus state))

  return $ not detectionEnabled
  
-- Updates the new global state depending on the detection results
newStatus :: DetectionStatus -> Status
newStatus DetectionOk      = StatusIdle
newStatus DetectionWrong   = StatusNotifying
newStatus DetectionUnknown = StatusFinding
