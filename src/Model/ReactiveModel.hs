-- | This module holds the reactive program model. It holds a program model,
-- but includes events that other threads can listen to, so that a change
-- in a part of the model is notified to another part of the program. The
-- reactive model is not necessarily concurrent (it doesn't have its own thread),
-- although a facility is included to make it also concurrent (so that
-- event handlers can be called as soon as they are present).
module Model.ReactiveModel
   ( ReactiveModel -- (basicModel, eventHandlers)
   -- * Construction
   , emptyRM
   -- * Access
   , pendingEvents
   , pendingHandlers
   -- * Modification
   , getPendingHandler
   , onEvent
   , module Exported
   )
  where

import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.Detector       as Exported
-- import Model.ReactiveModel.Initialisation as Exported
import Model.ReactiveModel.ModelEvents    as Exported
import Model.ReactiveModel.Preferences    as Exported
import Model.ReactiveModel.Sound          as Exported
import Model.ReactiveModel.Status         as Exported
