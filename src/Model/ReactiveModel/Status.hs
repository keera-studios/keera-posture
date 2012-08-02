-- | Manage the status field at reactive level
module Model.ReactiveModel.Status where

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

-- | Set the new status.
--
-- The status remains unmodified if a model invariant is violated.
setStatus :: ReactiveModel -> Status -> ReactiveModel
setStatus rm n
 -- Nothing has changed
 | getStatus rm == n = rm

 -- Wrong params
 | not notify && n == StatusNotifying = rm
 -- | notify && n == StatusCallibrating  = rm

 -- Ok
 | otherwise   = triggerEvent rm' ev
  where rm'    = rm `onBasicModel` (\b -> b { status = n })
        ev     = StatusChanged
        notify = notificationEnabled $ basicModel rm'

-- | Get the current system status
getStatus :: ReactiveModel -> Status
getStatus = status . basicModel
