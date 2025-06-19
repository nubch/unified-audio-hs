{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful ( runEff )
import Effectful.Dispatch.Static ( unsafeEff_ )
import Control.Concurrent (threadDelay)
import Audio ( playSound, stopSound, setVolume, setPanning )
import qualified Mock
import qualified SDL.SDL as SDL
import qualified Fmod.Fmod as Fmod
import qualified Interface as I

main :: IO ()
main = runEff $ SDL.runAudio do
  channel <- playSound @SDL.Channel "flim.mp3"
  unsafeEff_ $ threadDelay (3 * 1000000)
  _ <- setPanning channel (I.mkPanning (-1.0))
  unsafeEff_ $ threadDelay (3 * 1000000)
  _ <- setPanning channel (I.mkPanning (1.0))
  unsafeEff_ $ threadDelay (3 * 1000000)
  _ <- stopSound channel
  pure ()

