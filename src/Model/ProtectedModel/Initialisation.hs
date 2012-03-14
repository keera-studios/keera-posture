module Model.ProtectedModel.Initialisation where

import qualified Model.ReactiveModel as RM
import Model.ProtectedModel.ProtectedModelInternals

initialiseSystem :: ProtectedModel -> IO ()
initialiseSystem pm = applyToReactiveModel pm RM.initialiseSystem
