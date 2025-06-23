{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

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
main = runEff $ SDL.runAudio test

test :: (I.AudioEffect channel :> es) => Eff es ()
test = do
  ch0 <- playSound "flim.mp3"

  -- Pan ch1 completely to the left 
  delaySec 3
  setPanning ch0 (I.mkPanning (-1.0))
  pure ()
  where
    delaySec :: Int -> Eff es ()
    delaySec s = unsafeEff_ $ threadDelay (s * 1000000)
