-- | This module holds the functions to access and modify the sound being used
-- for notification 
module Model.ProtectedModel.Sound where

-- External imports
import qualified Control.Exception       as E
import           Control.Exception.Extra
import           Control.Monad

-- Internal imports
import           Audio
import           Model.ProtectedModel.ProtectedModelInternals
import qualified Model.ReactiveModel                          as RM
import           Control.Monad.IfElse
import           Paths

-- | Set the music to be played
setSound :: ProtectedModel -> Maybe Music -> IO()
setSound pm n = applyToReactiveModel pm (`RM.setSound` n)

-- | Get the music to be played
getSound :: ProtectedModel -> IO (Maybe Music)
getSound = (`onReactiveModel` RM.getSound)

-- | Set whether the music should be playing
setPlaying :: ProtectedModel -> Bool -> IO()
setPlaying pm n = applyToReactiveModel pm (`RM.setPlaying` n)

-- | Get whether the music should be playing
getPlaying :: ProtectedModel -> IO Bool
getPlaying = (`onReactiveModel` RM.getPlaying)

-- | Start playing the currently selected sound, if any
startPlaying :: ProtectedModel -> IO ()
startPlaying pm = do
  music   <- getSound pm
  playing <- getPlaying pm
  unless playing $ awhen music $ \music' -> do
    setPlaying pm True
    playMusic music'

-- | Stop playing any sound
stopPlaying :: ProtectedModel -> IO ()
stopPlaying pm = whenM (getPlaying pm) $ do
  setPlaying pm False
  stopMusic 
  
-- | Initialize the audio subsystem
startAudio :: ProtectedModel -> IO ()
startAudio pm =
  initAudio >> setSoundFilename pm Nothing

-- | Update the sound file to be played
setSoundFilename :: ProtectedModel -> Maybe FilePath -> IO()
setSoundFilename pm Nothing = do
  fn <- getDataFileName "warning.wav"
  m  <- E.handle (anyway (return Nothing)) $ fmap Just $ loadMusic fn
  setSound pm m
  applyToReactiveModel pm (`RM.setSoundFilename` Nothing)
setSoundFilename pm (Just fn) = do
  m <- E.handle (anyway (return Nothing)) $ fmap Just $ loadMusic fn
  setSound pm m
  applyToReactiveModel pm (`RM.setSoundFilename` Just fn)

-- | Get the name of the sound file to be played
getSoundFilename :: ProtectedModel -> IO (Maybe FilePath)
getSoundFilename = (`onReactiveModel` RM.getSoundFilename)
