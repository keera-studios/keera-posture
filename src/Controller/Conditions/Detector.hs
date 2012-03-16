module Controller.Conditions.Detector where

-- External imports
import           AI.CV.ImageProcessors
import           AI.CV.OpenCV.CV
import           AI.CV.OpenCV.CxCore
import           Control.Arrow
import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Exception.Extra
import           Control.Monad
import           Control.Processor       (processor, IOProcessor, IOSource)
import           Data.Maybe
import           Data.Time.Clock
import           Graphics.UI.Gtk         hiding (Image)
import           Foreign.C.Types
import           Hails.I18N.Gettext
import           Prelude                 hiding ((.),id)

-- Internal imports
import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive
import Paths

installHandlers :: CEnv -> IO()
installHandlers cenv = do
 let pm = model cenv
 onEvent pm Initialised         $ start cenv
 onEvent pm CameraChanged       $ restart cenv
 onEvent pm NotificationToggled $ start cenv

restart :: CEnv -> IO()
restart cenv = void $ forkIO $ do

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
 start cenv

-- | Starts a new thread when the detector is not running and
-- the current configuration allows it.
start :: CEnv -> IO ()
start cenv = void $ forkOS $ do

  -- First check if it should be started
  confOk         <- checkConfiguration cenv
  detectDisabled <- isDetectionDisabled cenv () -- undefined
  let pm = model cenv
  curDetector    <- getDetector pm

  when (confOk && not detectDisabled && not curDetector) $ do

    -- Mark detector running in the model
    setDetector pm True

    startDetector cenv

    -- Remove the pid from the model once it's finished
    setDetector pm False

-- Returns True if posture detection should be running.
isDetectionDisabled :: CEnv -> a -> IO Bool
isDetectionDisabled cenv _ = do
 let pm = model cenv
 v   <- getter notificationEnabledField pm
 cal <- getStatus pm
 return $ not v || cal >= StatusCallibrating

-- | Checks that the calibration parameters are defined
checkConfiguration :: CEnv -> IO Bool
checkConfiguration cenv = do
 let pm = model cenv
 cal <- getter calibrationParamsField pm
 return (isJust cal)

-- | Runs the detector with the configured parameters until it is disabled
startDetector :: CEnv -> IO ()
startDetector cenv = do
 let pm = model cenv
 cal <- getter calibrationParamsField pm
 del <- getter notificationDelayField  pm
 cor <- getter correctionFactorField   pm
 cam <- getter cameraField pm

 when (isJust cal) $ void $ do
   time <- getCurrentTime
   fn <- getDataFileName "haarcascade_frontalface_alt.xml"
   let (Just (x,y,w,h)) = cal
       state = (CvRect (fI x) (fI y) (fI w) (fI h), cor, del, 0, 0, time)
   E.handle (anyway $ wrongCam cenv) $ void $ do
     -- myPutStrLn "And here... we... go!"
     setter cameraStatusField pm (Just True)
     runTill (detector cam fn state cenv) (isDetectionDisabled cenv)
 where fI = fromIntegral

wrongCam :: CEnv -> IO()
wrongCam cenv = do
  let pm = model cenv
  setter notificationEnabledField pm False
  setter cameraStatusField pm (Just False)
  -- putStrLn "Wrong camera"

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
                      wrongCam cenv
                      showCalibrationResults cenv []
                  )) $ do
   -- Run for a few seconds or until once face is detected
   fn   <- getDataFileName "haarcascade_frontalface_alt.xml"
   cam  <- getter cameraField pm
   time <- getCurrentTime
   setter cameraStatusField pm (Just True)
   res  <- runTill (finder cam fn) (detectedOrFiveSeconds time)
   case res of
    [CvRect x y w h] -> do setter calibrationParamsField pm (Just (fI x, fI y, fI w, fI h))
                           setter notificationEnabledField pm enabled
    _                -> setter calibrationParamsField pm (Nothing :: Maybe (Int, Int, Int, Int))

    -- TODO: Place the image in the pixbuf
   showCalibrationResults cenv res

    -- TODO: Enable the buttons

 -- Maybe this should appear somewhere else? But careful with the previous handle
 setStatus pm StatusIdle
 where fI = fromIntegral :: CInt -> Int

showCalibrationResults :: CEnv -> [CvRect] -> IO()
showCalibrationResults cenv cvs = onViewAsync $ do

  -- ui <- fmap (mainWindowBuilder . view) $ readIORef cenv
  let ui = mainWindowBuilder $ view cenv

  lblT   <- calibrationTitleLbl ui
  lblE   <- calibrationExplanationLbl ui
  calImg <- calibrationImage ui
  if length cvs == 1
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

-- Returns true if exactly one rectangle has been detected
-- or 5 seconds have passed since 't'.
detectedOrFiveSeconds :: UTCTime -> [CvRect] -> IO Bool
detectedOrFiveSeconds t []  = do
  time <- getCurrentTime
  let diff = diffUTCTime time t
  return (diff > 5)
detectedOrFiveSeconds _ [_] = return True
detectedOrFiveSeconds _ _   = return False
  
-- Runs the face detector on the first camera
finder :: Int -> String -> IOProcessor () [CvRect]
finder n fn = camera n >>> haar fn

-- finder :: Int -> String -> IOProcessor () (Maybe Pixbuf, [CvRect])
-- finder n fn = camera n >>> ((id &&& h) &&& h) >>> ((drawRects >>> transformImage) *** id)
--  where h = haar fn

-- transformImage :: IOSource PImage (Maybe Pixbuf)
-- transformImage = processor
--   (\img' _ -> opencvImageToPixbuf img')
--   (\_      -> return Nothing)
--   (\x      -> return x)
--   (\_      -> return ())

-- Detects the current person, calculates its position and size
-- and notifies the system when necessary.
detector :: Int -> String -> InternalState -> CEnv -> IOProcessor () InternalState
detector n fn state cenv = camera n >>> haar fn >>> detectPosture cenv state

-- Internal state is
-- CvRect -> Head position when sitting correctly
-- Int    -> Correction factor
-- Int    -> Delay
-- Int    -> wrong frames in the past 'Delay'
-- Int    -> missing frames in the past 'Delay'
type InternalState = (CvRect, Int, Int, Int, Int, UTCTime)

-- Detects faces, compares with known references and notifies the system if the
-- user should correct the posture
detectPosture :: CEnv -> InternalState -> IOSource [CvRect] InternalState
detectPosture cenv initialState = processor
  (\rects state@(_, _, _, _, _, time) -> do
     -- Delays for several milisecons if the previous call happened
     -- not too long ago
     time' <- getCurrentTime
     let diff = diffUTCTime time' time
         wait = 1000000 * (0.8 - diff)
     when (wait > 0) $ threadDelay $ round wait

     -- Processes the current internal state and calculates the new one
     (rct, cr, d, w, m, _) <- process rects state

     -- Updates the new global state unless we are calibrating
     -- (which, incidentally, should never happen)
     let pm = model cenv
     st <- getStatus pm
     when (st /= StatusCallibrating) $ 
       -- Notify when wrong ~= delayWindow /\ wrong + missing > delayWindow
       case () of
        _ | w + m >= d && w > d - 2 -> setStatus pm StatusNotifying
          | m >= d                  -> setStatus pm StatusFinding
          | otherwise               -> setStatus pm StatusIdle

     -- Get a new timestamp and return the new state
     t' <- getCurrentTime
     return (rct, cr, d, w, m, t')
  )
  (\_           -> return initialState)
  (\_           -> return initialState)
  (\_           -> return ())

-- Calculates the new internal state from the previous one and the list of
-- detected faces
process :: [CvRect] -> InternalState -> IO InternalState
process es (rect, corr, delay, wrong, missing, time)
 | null es
 = return (rect, corr, delay, inRange (wrong - 1) (0, delay), inRange (missing + 1) (0, delay), time)
 | all (>= defaultMargin) corDistances
 = return (rect, corr, delay, inRange (wrong + 1) (0, delay), inRange (missing - 1) (0, delay), time)
 | otherwise -- any (< defaultMargin) corDistances
 = return (rect, corr, delay, 0, 0, time)
  where calculateDistance :: CvRect -> Double
        calculateDistance e = let (d1, d2) = calculateDistances e rect
                              in d1 * d2
        distances    :: [ Double ]
        distances    = map calculateDistance es
        -- corDistances :: 
        corDistances = map (\x -> x / (fromIntegral corr + 1)) distances

-- Face detector image processor
haar :: String -> IOProcessor Image [CvRect]
haar fn = haarDetect fn 1.1 3 (HaarDetectFlag 1) (CvSize 40 40)
        
-- This value is used to compare the calculated values from the 'correct' ones.
-- It is corrected (or relaxed) using a value in the internal state.
defaultMargin :: Double
defaultMargin = 500.0

-- Auxiliary operations

-- Calculates the distance between two rectangles in terms of position and
-- size.
calculateDistances :: CvRect -> CvRect -> (Double, Double)
calculateDistances r1 r2 = (d1, d2)
 where d1 = sqrt (f1 + f2)
       d2 = sqrt (f3 + f4)
       f1 = varDistance (rectX r1)      (rectX r2)      ^ (2 :: Int)
       f2 = varDistance (rectY r1)      (rectY r2)      ^ (2 :: Int)
       f3 = varDistance (rectWidth r1)  (rectWidth r2)  ^ (2 :: Int)
       f4 = varDistance (rectHeight r1) (rectHeight r2) ^ (2 :: Int)

-- Calculates the distance between two points
varDistance :: CInt -> CInt -> Double
varDistance v1 v2 = abs (fromIntegral v1 - fromIntegral v2)

-- inRance a (b1, b2) returns the closes value to a in the range [b1, b2]
inRange :: Int -> (Int, Int) -> Int
inRange x (a, b)
 | x >= a && x <= b = x
 | x < a            = a
 | otherwise        = b

title2 :: String
title2 = __ "<b>Calibration failed</b>"

title3 :: String
title3 = __ "<b>Calibration succeeded</b>"

explanation2 :: String
explanation2 = __ "Keera Posture was unable to detect you. Make sure that the camera is\n" ++
               __ "plugged and that you are sitting in front of the camera, neither too\n" ++ 
               __ "close nor too far."

explanation3 :: String
explanation3 = __ "Keera Posture has detected you successfully!\n" ++
               __ "You can close the assistant to proceed or\nrecalibrate if you were not sitting correctly."
