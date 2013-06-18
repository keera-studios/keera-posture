-- | List of all the conditions (changes in the model) that can be detected at
-- the reactive level
module Model.ReactiveModel.ModelEvents
 ( ModelEvent ( LanguageChanged
              , CheckUpdatesChanged
              , SoundFilenameChanged
              , SendReportsChanged
              , NotificationEnabledChanged
              , NotificationIconEnabledChanged
              , NotificationBubbleEnabledChanged
              , NotificationSoundEnabledChanged
              , NotificationOverlayEnabledChanged
              , NotificationDelayChanged
              , DetectionSlouchingEnabledChanged
              , DetectionHunchingEnabledChanged
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
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.Model.ProtectedModel.UpdatableModel

data ModelEvent = UncapturedEvent
                | LanguageChanged
                | SoundFilenameChanged
                | CheckUpdatesChanged
                | SendReportsChanged
                | NotificationEnabledChanged
                | NotificationIconEnabledChanged
                | NotificationBubbleEnabledChanged
                | NotificationSoundEnabledChanged
                | NotificationOverlayEnabledChanged
                | NotificationDelayChanged
                | DetectionSlouchingEnabledChanged
                | DetectionHunchingEnabledChanged
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

-- | The model can be used for automatic update discovery
instance UpdateNotifiableEvent ModelEvent where
  updateNotificationEvent = MaxVersionAvailable

-- | The model can be used for automatic initialisation notification
instance InitialisedEvent ModelEvent where
  initialisedEvent = Initialised
