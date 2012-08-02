-- | Determine whether the detector is executing
module Model.ReactiveModel.Detector where

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals

-- | Set whether the detector should be running
setDetector :: ReactiveModel -> Bool -> ReactiveModel
setDetector rm n
 -- Nothing has changed
 | getDetector rm == n = rm

 -- Ok
 | otherwise = rm `onBasicModel` (\b -> b { detectorRunning = n })

-- | Get whether the detector is running
getDetector :: ReactiveModel -> Bool
getDetector = detectorRunning . basicModel
