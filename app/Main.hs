module Main where

import Effectful
import Interface 
import SDL.SDL (backendSDL)
--import Fmod.Fmod (backendFmod)

main :: IO ()
main = runEff . runAudio backendSDL $ do
  initAudioB
  sound <- loadSound "flim.mp3"
  handle <- playSound
  stopSound