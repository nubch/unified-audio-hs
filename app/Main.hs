module Main where

import Control.Concurrent (threadDelay)
import Audio.Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))
import Audio.Fmod (backendFmod)
import Audio.SDL (backendSDL)

main :: IO ()
main = do
  let backend = backendFmod
  sys <- initAudio backend
  sound <- loadSound backend sys "example.wav"
  playing <- playSound backend sys sound
  _ <- getLine
  stopSound backend sys playing
