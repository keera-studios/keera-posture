module Model.ReactiveModel.Initialisation where

import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

initialiseSystem :: ReactiveModel -> ReactiveModel
initialiseSystem rm = triggerEvent rm Initialised
