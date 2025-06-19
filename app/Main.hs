{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful
import Effectful.Dispatch.Static ( unsafeEff_ )
import Control.Concurrent (threadDelay)
import Audio ( playSound, stopSound, setVolume, setPanning )
import qualified Mock
import qualified SDL.SDL as SDL
import qualified Fmod.Fmod as Fmod
import qualified Interface as I

main :: IO ()
main = runEff $ Fmod.runAudio do
  -- Play three sounds
  ch0 <- playSound @Fmod.Channel "flim.mp3"
  ch1 <- playSound @Fmod.Channel "flim.mp3"
  ch2 <- playSound @Fmod.Channel "flim.mp3"

  -- Pan ch1 completely to the left 
  delaySec 3
  setPanning ch1 (I.mkPanning (-1.0))

  -- Pan ch1 completely to the right
  delaySec 3
  stopSound ch1
  setPanning ch2 (I.mkPanning 1.0)

  -- Lower volume of ch0 
  delaySec 3
  stopSound ch1
  stopSound ch2
  setVolume ch0 (I.mkVolume (0.2))

  delaySec 3
  stopSound ch0

  pure ()
  where
    delaySec :: Int -> Eff es ()
    delaySec s = unsafeEff_ $ threadDelay (s * 1000000)
