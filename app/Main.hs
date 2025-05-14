module Main where

import Control.Concurrent (threadDelay)
import Audio.Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))
import Audio.Fmod
import Foreign
import Foreign.C.Types

main :: IO ()
main = alloca $ \pSystem -> do
  result1 <- c_FMOD_System_Create pSystem
  putStrLn $ "Create result: " ++ show result1

  system <- peek pSystem
  result2 <- c_FMOD_System_Init system 512 0 nullPtr
  putStrLn $ "Init result: " ++ show result2

