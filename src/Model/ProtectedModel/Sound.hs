-- | This module holds the functions to access and modify the project name
-- in a reactive model.
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

setSound :: ProtectedModel -> Maybe Music -> IO()
setSound pm n = applyToReactiveModel pm (`RM.setSound` n)

getSound :: ProtectedModel -> IO (Maybe Music)
getSound = (`onReactiveModel` RM.getSound)

setPlaying :: ProtectedModel -> Bool -> IO()
setPlaying pm n = applyToReactiveModel pm (`RM.setPlaying` n)

getPlaying :: ProtectedModel -> IO Bool
getPlaying = (`onReactiveModel` RM.getPlaying)

startPlaying :: ProtectedModel -> IO ()
startPlaying pm = do
  music   <- getSound pm
  playing <- getPlaying pm
  unless playing $ awhen music $ \music' -> do
    setPlaying pm True
    playMusic music'

stopPlaying :: ProtectedModel -> IO ()
stopPlaying pm = whenM (getPlaying pm) $ do
  setPlaying pm False
  stopMusic 
  
startAudio :: ProtectedModel -> IO ()
startAudio pm =
  initAudio >> setSoundFilename pm Nothing

setSoundFilename :: ProtectedModel -> Maybe FilePath -> IO()
setSoundFilename pm Nothing = do
  fn <- getDataFileName "warning.wav"
  loadMusic fn >>= setSound pm . Just
  applyToReactiveModel pm (`RM.setSoundFilename` Nothing)
setSoundFilename pm (Just fn) = do
  m <- E.handle (anyway (return Nothing)) $ fmap Just $ loadMusic fn
  setSound pm m
  applyToReactiveModel pm (`RM.setSoundFilename` Just fn)

getSoundFilename :: ProtectedModel -> IO (Maybe FilePath)
getSoundFilename = (`onReactiveModel` RM.getSoundFilename)
