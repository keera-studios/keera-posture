-- | This module defines the necessary operations to play music without having
-- to worry about SDL or import it. Other frameworks could provide similar
-- functions.
module Audio where

import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Music    as SDL.Mixer.Music
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types

type Music = SDL.Mixer.Types.Music

initAudio :: IO ()
initAudio = SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16Sys 2 4096

loadMusic :: String -> IO Music
loadMusic = SDL.Mixer.Music.loadMUS

playMusic :: Music -> IO ()
playMusic m = do SDL.Mixer.Music.setMusicVolume 50
                 SDL.Mixer.Music.playMusic m (-1)

stopMusic :: IO ()
stopMusic = SDL.Mixer.Music.haltMusic
