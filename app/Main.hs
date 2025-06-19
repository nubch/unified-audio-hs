{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful ( runEff )
import Effectful.Dispatch.Static ( unsafeEff_ )
import Control.Concurrent (threadDelay)
import Audio ( playSound, stopSound, setVolume )
--import qualified Mock
--import qualified SDL.SDL as SDL
import qualified Fmod.Fmod as Fmod
import qualified Interface as I

main :: IO ()
main = runEff $ Fmod.runAudio do
  channel <- playSound @Fmod.PlayingHandle "flim.mp3"
  unsafeEff_ $ threadDelay (1 * 1000000)
  _ <- setVolume (I.mkVolume 0.2) channel
  unsafeEff_ $ threadDelay (3 * 1000000)
  _ <- stopSound channel
  pure ()

