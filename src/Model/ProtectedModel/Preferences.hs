{-# LANGUAGE TemplateHaskell #-}
-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ProtectedModel.Preferences where

-- Internal imports
import           Hails.MVC.Model.THFields
import           Hails.MVC.Model.ProtectedModel.Reactive

import           Model.Model
import qualified Model.ReactiveModel                          as RM
import           Model.ReactiveModel.ModelEvents
import           Model.ProtectedModel.ProtectedModelInternals

protectedField "Language"                     [t|Maybe Language|]             "Model" "ModelEvent"
protectedField "CheckUpdates"                 [t|Bool|]                       "Model" "ModelEvent"
protectedField "SendReports"                  [t|Bool|]                       "Model" "ModelEvent"
protectedField "Camera"                       [t|Int|]                        "Model" "ModelEvent"
protectedField "CameraStatus"                 [t|Maybe Bool|]                 "Model" "ModelEvent"
protectedField "CalibrationParams"            [t|Maybe (Int, Int, Int, Int)|] "Model" "ModelEvent"
protectedField "CorrectionFactor"             [t|Int|]                        "Model" "ModelEvent"
protectedField "NotificationDelay"            [t|Int|]                        "Model" "ModelEvent"
protectedField "FirstRun"                     [t|Maybe Bool|]                 "Model" "ModelEvent"
protectedField "NotificationEnabled"          [t|Bool|]                       "Model" "ModelEvent"
protectedField "NotificationIconEnabled"      [t|Bool|]                       "Model" "ModelEvent"
protectedField "NotificationSoundEnabled"     [t|Bool|]                       "Model" "ModelEvent"
protectedField "NotificationOverlayEnabled"   [t|Bool|]                       "Model" "ModelEvent"
protectedField "DetectionSlouchingEnabled"    [t|Bool|]                       "Model" "ModelEvent"
protectedField "DetectionHunchingEnabled"     [t|Bool|]                       "Model" "ModelEvent"
