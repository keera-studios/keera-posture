module Model.ReactiveModel.Sound where

-- Internal imports
import Audio
import Model.Model
import Model.ReactiveModel.ModelEvents
import Model.ReactiveModel.ReactiveModelInternals

-- | Set the new status.
setSound :: ReactiveModel -> Maybe Music -> ReactiveModel
setSound rm n = rm `onBasicModel` (\b -> b { sound = n })

getSound :: ReactiveModel -> Maybe Music
getSound = sound . basicModel

-- | Set the new status.
setSoundFilename :: ReactiveModel -> Maybe FilePath -> ReactiveModel
setSoundFilename rm n'
 | n' /= n   = triggerEvent rm' SoundFilenameChanged
 | otherwise = rm
 where rm' = rm `onBasicModel` (\b -> b { soundFilename = n' })
       n   = soundFilename $ basicModel rm

getSoundFilename :: ReactiveModel -> Maybe FilePath
getSoundFilename = soundFilename . basicModel

-- | Set the new status.
setPlaying :: ReactiveModel -> Bool -> ReactiveModel
setPlaying rm n = rm `onBasicModel` (\b -> b { playing = n })

getPlaying :: ReactiveModel -> Bool
getPlaying = playing . basicModel
