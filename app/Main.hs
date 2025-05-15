module Main where

import Audio.Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))
import Audio.SDL (backendSDL)
import Audio.Fmod (backendFmod)

main :: IO ()
main = do
--let backend = backendFmod
  let backend = backendSDL
  sys     <- initAudio backend
  sound   <- loadSound backend sys "flim.mp3"
  playing <- playSound backend sys sound
  _       <- getLine
  stopSound backend sys playing
