-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ProtectedModel.Preferences where

-- Internal imports
import           Model.ProtectedModel.ProtectedModelInternals
import qualified Hails.MVC.Model.ProtectedModel.Reactive      as R
import           Hails.MVC.Model.ProtectedModel.Reactive      hiding (ReactiveElement)
import qualified Model.ReactiveModel                          as RM
import           Model.Model

type ReactiveElement a = R.ReactiveElement a Model RM.ModelEvent
-- | The check updates field
languageField :: ReactiveElement (Maybe Language)
languageField = R.ReactiveElement
 { reEvents = [ RM.LanguageChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setLanguage` c)
 , reGetter = (`onReactiveModel` RM.getLanguage)
 }

-- | The check updates field
checkUpdatesField :: ReactiveElement Bool
checkUpdatesField = R.ReactiveElement
 { reEvents = [ RM.CheckUpdatesChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCheckUpdates` c)
 , reGetter = (`onReactiveModel` RM.getCheckUpdates)
 }

-- | The send reports field
sendReportsField :: ReactiveElement Bool
sendReportsField = R.ReactiveElement
 { reEvents = [ RM.SendReportsChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setSendReports` c)
 , reGetter = (`onReactiveModel` RM.getSendReports)
 }

-- | The check updates field
notificationEnabledField :: ReactiveElement Bool
notificationEnabledField = R.ReactiveElement
 { reEvents = [ RM.NotificationToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationEnabled` c)
 , reGetter = (`onReactiveModel` RM.getNotificationEnabled)
 }

-- | The check updates field
notificationIconEnabledField :: ReactiveElement Bool
notificationIconEnabledField = R.ReactiveElement
 { reEvents = [ RM.NotificationIconToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationIconEnabled` c)
 , reGetter = (`onReactiveModel` RM.getNotificationIconEnabled)
 }

-- -- | The check updates field
-- notificationBubbleEnabledField :: ReactiveElement Bool
-- notificationBubbleEnabledField = ReactiveElement
--  { reEvents = [ RM.NotificationBubbleToggled ]
--  , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationBubbleEnabled` c)
--  , reGetter = (`onReactiveModel` RM.getNotificationBubbleEnabled)
--  }

-- | The check updates field
notificationSoundEnabledField :: ReactiveElement Bool
notificationSoundEnabledField = R.ReactiveElement
 { reEvents = [ RM.NotificationSoundToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationSoundEnabled` c)
 , reGetter = (`onReactiveModel` RM.getNotificationSoundEnabled)
 }

-- | The check updates field
notificationOverlayEnabledField :: ReactiveElement Bool
notificationOverlayEnabledField = R.ReactiveElement
 { reEvents = [ RM.NotificationOverlayToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationOverlayEnabled` c)
 , reGetter = (`onReactiveModel` RM.getNotificationOverlayEnabled)
 }

-- | The check updates field
detectionSlouchingEnabledField :: ReactiveElement Bool
detectionSlouchingEnabledField = R.ReactiveElement
 { reEvents = [ RM.DetectionSlouchingToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setDetectionSlouchingEnabled` c)
 , reGetter = (`onReactiveModel` RM.getDetectionSlouchingEnabled)
 }

-- | The check updates field
detectionHunchingEnabledField :: ReactiveElement Bool
detectionHunchingEnabledField = R.ReactiveElement
 { reEvents = [ RM.DetectionHunchingToggled ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setDetectionHunchingEnabled` c)
 , reGetter = (`onReactiveModel` RM.getDetectionHunchingEnabled)
 }

-- -- | The check updates field
-- cameraListField :: ReactiveElement [(Int, String)]
-- cameraListField = ReactiveElement
--  { reEvents = [ RM.CameraListChanged ]
--  , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCameraList` c)
--  , reGetter = (`onReactiveModel` RM.getCameraList)
--  }

-- | The check updates field
cameraField :: ReactiveElement Int
cameraField = R.ReactiveElement
 { reEvents = [ RM.CameraChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCamera` c)
 , reGetter = (`onReactiveModel` RM.getCamera)
 }

-- | The check updates field
cameraStatusField :: ReactiveElement (Maybe Bool)
cameraStatusField = R.ReactiveElement
 { reEvents = [ RM.CameraStatusChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCameraStatus` c)
 , reGetter = (`onReactiveModel` RM.getCameraStatus)
 }

-- | The check updates field
calibrationParamsField :: ReactiveElement (Maybe (Int, Int, Int, Int))
calibrationParamsField = R.ReactiveElement
 { reEvents = [ RM.CalibrationParamsChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCalibrationParams` c)
 , reGetter = (`onReactiveModel` RM.getCalibrationParams)
 }

-- | The check updates field
correctionFactorField :: ReactiveElement Int
correctionFactorField = R.ReactiveElement
 { reEvents = [ RM.CorrectionFactorChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setCorrectionFactor` c)
 , reGetter = (`onReactiveModel` RM.getCorrectionFactor)
 }

-- | The check updates field
notificationDelayField :: ReactiveElement Int
notificationDelayField = R.ReactiveElement
 { reEvents = [ RM.NotificationDelayChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (`RM.setNotificationDelay` c)
 , reGetter = (`onReactiveModel` RM.getNotificationDelay)
 }

-- | Is it the first run?
firstRunField :: ReactiveElement (Maybe Bool)
firstRunField = R.ReactiveElement
 { reEvents = [ RM.FirstRunChanged ]
 , reSetter = \pm c -> pm `applyToReactiveModel` (\v -> RM.fieldSetter RM.firstRunField v c)
 , reGetter = (`onReactiveModel` RM.fieldGetter RM.firstRunField)
 }
