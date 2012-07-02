module Model.Model where

import Data.ExtraVersion
import Hails.MVC.Model.ProtectedModel.VersionedModel
import Hails.MVC.Model.ProtectedModel.UpdatableModel

import Audio

-- | Model invariants
--
-- Condition: At least one notification method must be enabled at all times
--
-- INV: x == notificationMethods
--   /\ (  x.notificationIconEnabled
--      \/ x.notificationBubbleEnabled
--      \/ x.notificationSoundEnabled
--      \/ x.notificationOverlayEnabled
--      )
--
-- Condition: At least one detection method must be enabled at all times
--
-- INV: x == detectionMethods
--   /\ (  x.detectionSlouchingEnabled
--      \/ x.detectionHunchingEnabled
--      )
--
-- INV: notificationDelay >= 1
--
-- INV: camera == Nothing || camera == (Just (x,_)) && x >= 0
--
-- INV:    calibrationParams == Nothing
--      || calibrationParams == (Just (x,y,w,h)) && x >= 0 && y >= 0 && w >= 0 && h >= 0
--
-- INV: correctionFactor in [0,10]
--
-- INV: not (notificationEnabled /\ status == Callibrating)
--
-- INV: not (not notificationEnabled /\ status == Notifying)

data Model = Model
 {
 -- General settings
   language            :: Maybe Language
 , checkUpdates        :: Bool
 , sendReports         :: Bool

 -- Notification params
 , notificationEnabled :: Bool
 , notificationMethods :: NotificationMethods
 , notificationDelay   :: Int

 -- Algorithm params
 , detectionMethods    :: DetectionMethods
 , camera              :: Int
 , cameraStatus        :: Maybe Bool

 -- , cameraList          :: [(Int, String)]
 , calibrationParams   :: Maybe (Int, Int, Int, Int)
 , correctionFactor    :: Int

 -- Current status
 , status              :: Status

 , detectorRunning     :: Bool

 -- Sound related
 , sound               :: Maybe Music
 , soundFilename       :: Maybe FilePath
 , playing             :: Bool

 -- Updating system 
 , programName         :: String
 , programVersion      :: Version
 , programMaxVersion   :: Maybe Version
 , programUpdateURI    :: String

 , firstRun            :: Maybe Bool -- Unknown, Yes, No
 }
 deriving (Eq)

data NotificationMethods = NotificationMethods
 { notificationIconEnabled    :: Bool
 -- , notificationBubbleEnabled  :: Bool
 , notificationSoundEnabled   :: Bool
 , notificationOverlayEnabled :: Bool
 }
 deriving (Eq)

data DetectionMethods = DetectionMethods 
 { detectionSlouchingEnabled :: Bool
 , detectionHunchingEnabled  :: Bool
 }
 deriving (Eq)

data Status = StatusIdle
            | StatusFinding
            | StatusNotifying
            | StatusCallibrating
            | StatusDisabled
 deriving (Eq, Ord)

data Language = English
              | Galician
              | Spanish
 deriving (Eq, Show)

emptyBM :: Model
emptyBM = Model
  {
  -- General settings
    language            = Just English
  , checkUpdates        = True
  , sendReports         = True
 
  -- Notification params
  , notificationEnabled = False
  , notificationMethods = NotificationMethods { notificationIconEnabled    = True
                                              -- , notificationBubbleEnabled  = True
                                              , notificationSoundEnabled   = True
                                              , notificationOverlayEnabled = True
                                              }
  , notificationDelay   = 8
 
  -- Algorithm params
  , detectionMethods    = DetectionMethods { detectionSlouchingEnabled = True
                                           , detectionHunchingEnabled  = True
                                           }
 
  , camera              = 0
  , cameraStatus        = Nothing

  -- , cameraList          = []
  , calibrationParams   = Nothing -- Just (287, 122, 232, 232)
  , correctionFactor    = 0
 
  -- Current status
  , status              = StatusIdle
  , detectorRunning     = False
 
  -- Sound
  , sound               = Nothing
  , soundFilename       = Nothing
  , playing             = False
 
  -- Update check
  , programName         = "Keera Posture"
  , programVersion      = Version 0 0 Beta 0
  , programMaxVersion   = Nothing
  , programUpdateURI    = "http://www.keera.es/projects/keera-posture/newest-version"
 
  , firstRun            = Nothing
  }

instance VersionedBasicModel Model where
  getBMVersion = programVersion

instance UpdatableBasicModel Model where
  getBMUpdateURI            = programUpdateURI
  getBMMaxVersionAvail      = programMaxVersion
  setBMMaxVersionAvail bm x = bm { programMaxVersion = Just x }
