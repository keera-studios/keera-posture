-- | Determines whether the detection thread is running
module Model.ProtectedModel.Detector
   ( getDetector
   , setDetector
   , waitForDetector
   )
  where

-- Internal imports
import Model.ProtectedModel.ProtectedModelInternals
import qualified Model.ReactiveModel as RM
import Data.ReactiveValue

-- | Request to start/stop the detection thread
setDetector :: ProtectedModel -> Bool -> IO()
setDetector pm n = applyToReactiveModel pm (`RM.setDetector` n)

-- | Get whether the detection thread is running
getDetector :: ProtectedModel -> IO Bool
getDetector = (`onReactiveModel` RM.getDetector)

-- detectorField :: ReactiveFieldReadWrite IO Bool

-- | Lock thread until the detection thread is dead
waitForDetector :: ProtectedModel -> IO ()
waitForDetector pm = waitFor pm cond
 where cond = not . RM.getDetector
