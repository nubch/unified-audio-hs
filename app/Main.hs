module Main where

import Effectful
import Effectful.Dispatch.Static
import Interface 
import Control.Concurrent (threadDelay)
--import Mock
import SDL.SDL
--import Fmod.Fmod (backendFmod)

main :: IO ()
main = runEff $ runAudio (do
  _ <- playSound "flim.mp3"
  unsafeEff_ $ threadDelay (3 * 1000000))
  --stopSound w )
