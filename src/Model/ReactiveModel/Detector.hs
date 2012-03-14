module Model.ReactiveModel.Detector where

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals

-- | Set the new status.
setDetector :: ReactiveModel -> Bool -> ReactiveModel
setDetector rm n
 -- Nothing has changed
 | getDetector rm == n = rm

 -- Ok
 | otherwise = rm `onBasicModel` (\b -> b { detectorRunning = n })

getDetector :: ReactiveModel -> Bool
getDetector = detectorRunning . basicModel
