module Controller.Conditions.Calibration (calibrate) where

-- External imports
import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Exception.Extra
import           Control.Monad
import           Data.Maybe
import           Graphics.UI.Gtk         hiding (Image)

-- Internal imports
import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive
import Paths
import I18N.Strings
import AI.CV.PostureProcessors

-- The following function is executed in a separate thread so that the
-- notification viewers are updated and the system remains responsive.
calibrate :: CEnv -> IO()
calibrate cenv = void $ forkOS $ do

 let pm = model cenv
 setStatus pm StatusCallibrating

 -- Remember if the system was enabled and restore the same state later.
 enabled <- getter notificationEnabledField pm
 setter notificationEnabledField pm False

 -- This value is completely random. It simply works, but there must be a
 -- better way.
 threadDelay 1000000

 -- What for the detector to die
 waitForDetector pm

 E.handle (anyway (do setter calibrationParamsField pm (Nothing :: Maybe (Int, Int, Int, Int))
                      wrongCam pm
                      showCalibrationResults cenv Nothing
                  )) $ do

   -- Run for a few seconds or until once face is detected
   cam <- getter cameraField pm
   res <- runFinder cam
   setter calibrationParamsField pm res

   -- If calibration went ok, reset the previous notification status
   when (isJust res) $ setter notificationEnabledField pm enabled

    -- TODO: Place the image in the pixbuf
   showCalibrationResults cenv res

    -- TODO: Enable the buttons

 -- Maybe this should appear somewhere else? But careful with the previous handle
 setStatus pm StatusIdle

showCalibrationResults :: CEnv -> Maybe (Int, Int, Int, Int) -> IO()
showCalibrationResults cenv cvs = onViewAsync $ do
  let ui = mainWindowBuilder $ view cenv

  lblT   <- calibrationTitleLbl ui
  lblE   <- calibrationExplanationLbl ui
  calImg <- calibrationImage ui
  if isJust cvs
   then do fn <- getDataFileName "calibration-ok.png"
           imageSetFromFile calImg fn
           labelSetMarkup lblT title3
           labelSetText lblE explanation3
   else do fn <- getDataFileName "calibration-wrong.png"
           imageSetFromFile calImg fn
           labelSetMarkup lblT title2
           labelSetText lblE explanation2
    
  -- Showing the capture on the screen
  -- when (isJust img && length cvs == 1) $
  --   imageSetFromPixbuf calImg (fromJust img)

  btn1 <- calibrationCloseBtn ui
  btn2 <- calibrationRecalibrateBtn ui

  widgetSetSensitive btn1 True
  widgetSetSensitive btn2 True
