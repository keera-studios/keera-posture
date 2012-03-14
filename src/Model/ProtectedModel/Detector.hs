-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ProtectedModel.Detector
   ( getDetector
   , setDetector
   , waitForDetector
   )
  where

-- Internal imports
import Model.ProtectedModel.ProtectedModelInternals
import qualified Model.ReactiveModel as RM

setDetector :: ProtectedModel -> Bool -> IO()
setDetector pm n = applyToReactiveModel pm (`RM.setDetector` n)

getDetector :: ProtectedModel -> IO Bool
getDetector = (`onReactiveModel` RM.getDetector)

waitForDetector :: ProtectedModel -> IO ()
waitForDetector pm = waitFor pm cond
 where cond = not . RM.getDetector
