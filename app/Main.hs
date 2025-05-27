module Main where

import Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))
import SDL.SDL (backendSDL)
import Fmod.Fmod (backendFmod)

main :: IO ()
main = do
  let backend = backendFmod
--let backend = backendSDL
  sys     <- initAudio backend
  sound   <- loadSound backend sys "flim.mp3"
  playing <- playSound backend sys sound
  _       <- getLine
  stopSound backend sys playing
