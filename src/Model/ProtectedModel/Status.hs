-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ProtectedModel.Status
   ( getStatus
   , setStatus
   )
  where

-- Internal imports
import Model.Model
import Model.ProtectedModel.ProtectedModelInternals
import qualified Model.ReactiveModel as RM

setStatus :: ProtectedModel -> Status -> IO()
setStatus pm n = applyToReactiveModel pm (`RM.setStatus` n)

getStatus :: ProtectedModel -> IO Status
getStatus = (`onReactiveModel` RM.getStatus)
