{-# LANGUAGE TemplateHaskell #-}
-- | Access the system's status in a thread-safe way
module Model.ProtectedModel.Status where

import           Hails.MVC.Model.THFields
import           Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import           Model.Model
import qualified Model.ReactiveModel                          as RM
import           Model.ReactiveModel.ModelEvents
import           Model.ProtectedModel.ProtectedModelInternals

protectedField "Status" [t|Status|] "Model" "ModelEvent"
