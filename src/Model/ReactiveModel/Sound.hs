-- | Sound field management at reactive level
module Model.ReactiveModel.Sound where

-- Internal imports
import Audio
import Model.Model
import Model.ReactiveModel.ModelEvents
import Model.ReactiveModel.ReactiveModelInternals

-- | Set the sound to be played when notifying
setSound :: ReactiveModel -> Maybe Music -> ReactiveModel
setSound rm n = rm `onBasicModel` (\b -> b { sound = n })

-- | Get the sound to be played when notifying
getSound :: ReactiveModel -> Maybe Music
getSound = sound . basicModel

-- | Set the sound filename to be played when notifying
setSoundFilename :: ReactiveModel -> Maybe FilePath -> ReactiveModel
setSoundFilename rm n'
 | n' /= n   = triggerEvent rm' SoundFilenameChanged
 | otherwise = rm
 where rm' = rm `onBasicModel` (\b -> b { soundFilename = n' })
       n   = soundFilename $ basicModel rm

-- | Get the sound filename to be played when notifying
getSoundFilename :: ReactiveModel -> Maybe FilePath
getSoundFilename = soundFilename . basicModel

-- | Set whether the sound is currently playing
setPlaying :: ReactiveModel -> Bool -> ReactiveModel
setPlaying rm n = rm `onBasicModel` (\b -> b { playing = n })

-- | Get current sound playing status
getPlaying :: ReactiveModel -> Bool
getPlaying = playing . basicModel
