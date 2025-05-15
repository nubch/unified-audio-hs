module Main where

import Control.Concurrent (threadDelay)
import Audio.Interface (AudioBackend(initAudio, playSound, loadSound, stopSound))
import Audio.Fmod
import Foreign
import Foreign.C.Types

main :: IO ()
main = alloca $ \pSystem -> do
  let fmodVersion = 0x00020221 :: CUInt
  result1 <- c_FMOD_System_Create pSystem fmodVersion
  print result1

  system <- peek pSystem
  let fmod_INIT_NORMAL = 0x00000000 :: CInt
  result2 <- c_FMOD_System_Init system 512 fmod_INIT_NORMAL nullPtr
  print result2