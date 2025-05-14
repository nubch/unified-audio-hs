module Main where

import Audio.SDL
import Control.Concurrent (threadDelay)
import Audio.Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))

main :: IO ()
main = do
    let backend = backendSDL
    initAudio backend
    sound   <- loadSound backend "example.wav" 
    channel <- playSound backend sound
    threadDelay (2 * 1000000)
    stopSound backend channel
