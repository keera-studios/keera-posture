module Model.ReactiveModel.ModelEvents
 ( ModelEvent ( LanguageChanged
              , CheckUpdatesChanged
              , SoundFilenameChanged
              , SendReportsChanged
              , NotificationToggled
              , NotificationIconToggled
              , NotificationBubbleToggled
              , NotificationSoundToggled
              , NotificationOverlayToggled
              , NotificationDelayChanged
              , DetectionSlouchingToggled
              , DetectionHunchingToggled
              , CameraChanged
              , CameraStatusChanged
              -- , CameraListChanged
              , CalibrationParamsChanged
              , CorrectionFactorChanged
              , StatusChanged 
              , PlayingStatusChanged
              , Initialised
              , MaxVersionAvailable
              , FirstRunChanged 
              )
 ) where

import qualified Hails.MVC.Model.ReactiveModel as GRM
import Hails.MVC.Model.ProtectedModel.UpdatableModel

data ModelEvent = UncapturedEvent
                | LanguageChanged
                | SoundFilenameChanged
                | CheckUpdatesChanged
                | SendReportsChanged
                | NotificationToggled
                | NotificationIconToggled
                | NotificationBubbleToggled
                | NotificationSoundToggled
                | NotificationOverlayToggled
                | NotificationDelayChanged
                | DetectionSlouchingToggled
                | DetectionHunchingToggled
                | CameraChanged
                | CameraStatusChanged
                -- | CameraListChanged
                | CalibrationParamsChanged
                | CorrectionFactorChanged
                | StatusChanged 
                | PlayingStatusChanged
                | Initialised
                | MaxVersionAvailable
                | FirstRunChanged
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent

instance UpdateNotifiableEvent ModelEvent where
  updateNotificationEvent = MaxVersionAvailable
