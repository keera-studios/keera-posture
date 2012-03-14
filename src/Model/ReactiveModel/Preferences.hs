-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ReactiveModel.Preferences
   ( getLanguage
   , setLanguage
   , setCheckUpdates
   , getCheckUpdates
   , setSendReports
   , getSendReports
   , setNotificationEnabled
   , getNotificationEnabled
   , setNotificationIconEnabled
   , getNotificationIconEnabled
   -- , setNotificationBubbleEnabled
   -- , getNotificationBubbleEnabled
   , setNotificationSoundEnabled
   , getNotificationSoundEnabled
   , setNotificationOverlayEnabled
   , getNotificationOverlayEnabled
   , setNotificationDelay
   , getNotificationDelay
   , setDetectionSlouchingEnabled
   , getDetectionSlouchingEnabled
   , setDetectionHunchingEnabled
   , getDetectionHunchingEnabled
   , setCamera
   , getCamera
   , setCameraStatus
   , getCameraStatus
   -- , setCameraList
   -- , getCameraList
   , setCalibrationParams
   , getCalibrationParams
   , setCorrectionFactor
   , getCorrectionFactor
   , firstRunField
   , fieldSetter
   , fieldGetter
   )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

-- The following code presents a possibly simpler way of creating reactive
-- fields in a reactive model.
type Field a = (Model -> a, a -> Model -> Bool, a -> Model -> Model, ModelEvent)

preTrue :: a -> Model -> Bool
preTrue = const $ const True

fieldSetter :: Eq a => Field a -> ReactiveModel -> a -> ReactiveModel
fieldSetter f@(_, pre, rSet, ev) rm newVal
  | fieldGetter f rm == newVal       = rm
  | not $ pre newVal $ basicModel rm = triggerEvent rm ev
  | otherwise                        = triggerEvent rm' ev
 where rm' = rm `onBasicModel` rSet newVal

fieldGetter :: Field a -> ReactiveModel -> a
fieldGetter (rGet,_,_,_) = rGet . basicModel

-- | Basic settings
setLanguage :: ReactiveModel -> Maybe Language -> ReactiveModel
setLanguage = fieldSetter languageField

getLanguage :: ReactiveModel -> Maybe Language
getLanguage = fieldGetter languageField

-- | The internal field declaration
languageField :: Field (Maybe Language)
languageField = (language, preTrue, \v b -> b { language = v}, LanguageChanged)

-- | Basic settings
-- setLanguage :: ReactiveModel -> Maybe Language -> ReactiveModel
-- setLanguage rm n
--  | getLanguage rm == n = rm
--  | otherwise           = triggerEvent rm' ev
--   where rm' = rm `onBasicModel` (\b -> b { language = n })
--         ev  = LanguageChanged
-- 
-- getLanguage :: ReactiveModel -> Maybe Language
-- getLanguage = language . basicModel

setCheckUpdates :: ReactiveModel -> Bool -> ReactiveModel
setCheckUpdates = fieldSetter checkUpdatesField

getCheckUpdates :: ReactiveModel -> Bool
getCheckUpdates = fieldGetter checkUpdatesField

checkUpdatesField :: Field Bool
checkUpdatesField = (checkUpdates, preTrue, \v b -> b { checkUpdates = v}, CheckUpdatesChanged)

-- setCheckUpdates :: ReactiveModel -> Bool -> ReactiveModel
-- setCheckUpdates rm n
--  | getCheckUpdates rm == n = rm
--  | otherwise          = triggerEvent rm' ev
--   where rm' = rm `onBasicModel` (\b -> b { checkUpdates = n })
--         ev  = CheckUpdatesChanged

-- getCheckUpdates :: ReactiveModel -> Bool
-- getCheckUpdates = checkUpdates . basicModel

setSendReports :: ReactiveModel -> Bool -> ReactiveModel
setSendReports rm n
 | getSendReports rm == n = rm
 | otherwise              = triggerEvent rm' ev
  where rm' = rm `onBasicModel` (\b -> b { sendReports = n })
        ev  = SendReportsChanged

getSendReports :: ReactiveModel -> Bool
getSendReports = sendReports . basicModel

-- | Notification params

-- | Should the system notify the user?
setNotificationEnabled :: ReactiveModel -> Bool -> ReactiveModel
setNotificationEnabled rm n
 -- Nothing has changed
 | getNotificationEnabled rm == n = rm
 -- Wrong params
 | not n && st == StatusNotifying = rm
 | n     && isNothing cal         = rm
 -- Ok
 | otherwise                      = triggerEvent rm' ev
  where rm' = rm `onBasicModel` (\b -> b { notificationEnabled = n })
        ev  = NotificationToggled
        st  = status $ basicModel rm
        cal = getCalibrationParams rm

getNotificationEnabled :: ReactiveModel -> Bool
getNotificationEnabled = notificationEnabled . basicModel

-- | Should the icon change to notify the user?
setNotificationIconEnabled :: ReactiveModel -> Bool -> ReactiveModel
setNotificationIconEnabled rm n
 -- Nothing has changed
 | getNotificationIconEnabled rm == n = rm

 -- Incorrect change
 | noNotificationMethod nm' = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { notificationMethods = nm' })
        nm'  = nm { notificationIconEnabled = n }
        nm   = notificationMethods $ basicModel rm
        ev   = NotificationIconToggled

getNotificationIconEnabled :: ReactiveModel -> Bool
getNotificationIconEnabled = notificationIconEnabled . notificationMethods . basicModel

-- -- | Should the icon change to notify the user?
-- setNotificationBubbleEnabled :: ReactiveModel -> Bool -> ReactiveModel
-- setNotificationBubbleEnabled rm n
--  -- Nothing has changed
--  | getNotificationBubbleEnabled rm == n = rm
-- 
--  -- Incorrect change
--  | noNotificationMethod nm' = triggerEvent rm ev
--  
--  -- Ok
--  | otherwise = triggerEvent rm' ev
--   where rm'  = rm `onBasicModel` (\b -> b { notificationMethods = nm' })
--         nm'  = nm { notificationBubbleEnabled = n }
--         nm   = notificationMethods $ basicModel rm
--         ev   = NotificationBubbleToggled
-- 
-- getNotificationBubbleEnabled :: ReactiveModel -> Bool
-- getNotificationBubbleEnabled = notificationBubbleEnabled . notificationMethods . basicModel

-- | Should the icon change to notify the user?
setNotificationSoundEnabled :: ReactiveModel -> Bool -> ReactiveModel
setNotificationSoundEnabled rm n
 -- Nothing has changed
 | getNotificationSoundEnabled rm == n = rm

 -- Incorrect change
 | noNotificationMethod nm' = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { notificationMethods = nm' })
        nm'  = nm { notificationSoundEnabled = n }
        nm   = notificationMethods $ basicModel rm
        ev   = NotificationSoundToggled

getNotificationSoundEnabled :: ReactiveModel -> Bool
getNotificationSoundEnabled = notificationSoundEnabled . notificationMethods . basicModel

-- | Should the icon change to notify the user?
setNotificationOverlayEnabled :: ReactiveModel -> Bool -> ReactiveModel
setNotificationOverlayEnabled rm n
 -- Nothing has changed
 | getNotificationOverlayEnabled rm == n = rm

 -- Incorrect change
 | noNotificationMethod nm' = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { notificationMethods = nm' })
        nm'  = nm { notificationOverlayEnabled = n }
        nm   = notificationMethods $ basicModel rm
        ev   = NotificationOverlayToggled

getNotificationOverlayEnabled :: ReactiveModel -> Bool
getNotificationOverlayEnabled = notificationOverlayEnabled . notificationMethods . basicModel

-- | Change notification delay (time between detection and notification)
setNotificationDelay :: ReactiveModel -> Int -> ReactiveModel
setNotificationDelay rm n
 -- Nothing has changed
 | getNotificationDelay rm == n = rm

 -- Incorrect change
 | n < 1 = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { notificationDelay = n })
        ev   = NotificationDelayChanged

getNotificationDelay :: ReactiveModel -> Int
getNotificationDelay = notificationDelay . basicModel

-- | Should the icon change to notify the user?
setDetectionSlouchingEnabled :: ReactiveModel -> Bool -> ReactiveModel
setDetectionSlouchingEnabled rm n
 -- Nothing has changed
 | getDetectionSlouchingEnabled rm == n = rm

 -- Incorrect change
 | noDetectionMethod dm' = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { detectionMethods = dm' })
        dm'  = dm { detectionSlouchingEnabled = n }
        dm   = detectionMethods $ basicModel rm
        ev   = DetectionSlouchingToggled

getDetectionSlouchingEnabled :: ReactiveModel -> Bool
getDetectionSlouchingEnabled = detectionSlouchingEnabled . detectionMethods . basicModel

-- | Should the icon change to notify the user?
setDetectionHunchingEnabled :: ReactiveModel -> Bool -> ReactiveModel
setDetectionHunchingEnabled rm n
 -- Nothing has changed
 | getDetectionHunchingEnabled rm == n = rm

 -- Incorrect change
 | noDetectionMethod dm' = triggerEvent rm ev
 
 -- Ok
 | otherwise = triggerEvent rm' ev
  where rm'  = rm `onBasicModel` (\b -> b { detectionMethods = dm' })
        dm'  = dm { detectionHunchingEnabled = n }
        dm   = detectionMethods $ basicModel rm
        ev   = DetectionHunchingToggled

getDetectionHunchingEnabled :: ReactiveModel -> Bool
getDetectionHunchingEnabled = detectionHunchingEnabled . detectionMethods . basicModel

-- Auxiliary function that should not be exported
noNotificationMethod :: NotificationMethods -> Bool
noNotificationMethod nm =
 not $  notificationIconEnabled  nm -- || notificationBubbleEnabled  nm
     || notificationSoundEnabled nm || notificationOverlayEnabled nm

-- Auxiliary function that should not be exported
noDetectionMethod :: DetectionMethods -> Bool
noDetectionMethod nm =
 not $ detectionSlouchingEnabled nm || detectionHunchingEnabled nm

-- -- | Change the camera selection
-- --
-- -- The model remains unchanged if the list is the same that the model holds
-- -- currently or if any id is negative.
-- setCameraList :: ReactiveModel -> [(Int, String)] -> ReactiveModel
-- setCameraList rm n
--  | getCameraList rm == n = rm
--  | all ((0 <=) . fst) n  = triggerEvent rm ev
--  | otherwise             = triggerEvent rm' ev
--   where rm' = rm `onBasicModel` (\b -> b { cameraList = n })
--         ev  = CameraListChanged
-- 
-- getCameraList :: ReactiveModel -> [(Int, String)]
-- getCameraList = cameraList . basicModel


-- Invariant: it cannot be the case that the camera is changed
-- and the status remains set.

-- | Change the camera selection
setCamera :: ReactiveModel -> Int -> ReactiveModel
setCamera rm n
 | getCamera rm == n     = rm
 | n < 0                 = triggerEvent rm ev
 | otherwise             = setCameraStatus rm'' Nothing
  where rm'' = triggerEvent rm' ev
        rm'  = rm `onBasicModel` (\b -> b { camera = n })
        ev   = CameraChanged

getCamera :: ReactiveModel -> Int
getCamera = camera . basicModel

-- | Change the camera selection
setCameraStatus :: ReactiveModel -> Maybe Bool -> ReactiveModel
setCameraStatus rm n
 | getCameraStatus rm == n = rm
 | otherwise               = triggerEvent rm' ev
  where rm' = rm `onBasicModel` (\b -> b { cameraStatus = n })
        ev  = CameraStatusChanged

getCameraStatus :: ReactiveModel -> Maybe Bool
getCameraStatus = cameraStatus . basicModel

-- | Change Calibration parameters
setCalibrationParams :: ReactiveModel -> Maybe (Int, Int, Int, Int) -> ReactiveModel
setCalibrationParams rm n
 -- Nothing has changed
 | getCalibrationParams rm == n                   = rm

 -- Wrong parameters
 | isJust n && (x < 0 || y < 0 || w < 0 || h < 0) = triggerEvent rm ev

 -- -- It's set to nothing, so the notification cannot be enabled
 | isNothing n && det                             = setNotificationEnabled rm'' False
 -- Ok
 | otherwise = rm''
  where rm'' = triggerEvent rm' ev
        rm'  = rm `onBasicModel` (\b -> b { calibrationParams = n })
        ev   = CalibrationParamsChanged
        det  = getNotificationEnabled rm
        (Just (x, y, w, h)) = n

getCalibrationParams :: ReactiveModel -> Maybe (Int, Int, Int, Int)
getCalibrationParams = calibrationParams . basicModel

-- | Change Callibration parameters
setCorrectionFactor :: ReactiveModel -> Int -> ReactiveModel
setCorrectionFactor rm n
 -- Nothing has changed
 | getCorrectionFactor rm == n = rm

 -- Wrong parameters
 | n < 0 || n > 10             = triggerEvent rm ev

 -- Ok
 | otherwise                   = triggerEvent rm' ev
  where rm' = rm `onBasicModel` (\b -> b { correctionFactor = n })
        ev  = CorrectionFactorChanged

getCorrectionFactor :: ReactiveModel -> Int
getCorrectionFactor = correctionFactor . basicModel

-- | Expliration field at the reactive level
firstRunField :: Field (Maybe Bool)
firstRunField = (firstRun, preTrue, \v b -> b { firstRun = v}, FirstRunChanged)
