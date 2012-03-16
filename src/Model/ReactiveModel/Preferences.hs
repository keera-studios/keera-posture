{-# LANGUAGE TemplateHaskell #-}
-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ReactiveModel.Preferences where

-- External imports
import Data.Maybe
import qualified Hails.MVC.Model.ReactiveFields as RFs
import Hails.MVC.Model.ReactiveFields 
         (fieldGetter, fieldSetter, preTrue)
import Hails.MVC.Model.THFields

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

-- A Field of type A lets us access a reactive field of type a from
-- a Model, and it triggers a ModelEvent
type Field a = RFs.Field a Model ModelEvent

-- | Basic settings
reactiveField "Language"     [t|Maybe Language|]
reactiveField "CheckUpdates" [t|Bool|]
reactiveField "SendReports"  [t|Bool|]
reactiveField "FirstRun"     [t|Maybe Bool|]

-- | Settings with non-true preconditions for set functions

-- | Notification params
--
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
        ev  = NotificationEnabledChanged
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
        ev   = NotificationIconEnabledChanged

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
--         ev   = NotificationBubbleChanged
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
        ev   = NotificationSoundEnabledChanged

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
        ev   = NotificationOverlayEnabledChanged

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
        ev   = DetectionSlouchingEnabledChanged

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
        ev   = DetectionHunchingEnabledChanged

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
