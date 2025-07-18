{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Effectful
import Effectful.Dispatch.Static ( unsafeEff_, unsafeEff )
import Control.Concurrent (threadDelay)
import qualified Mock
import Control.Concurrent
import qualified SDL.Backend as SDL
import qualified Fmod.Backend as Fmod
import UnifiedAudio.Effectful

main :: IO ()
main = runEff $ Fmod.runAudio test

test :: (Audio channel :> es) => Eff es ()
test = do
  sound <- load "flim.mp3"
  sound2 <- load "flim.mp3"
  playing <- play sound
  playing2 <- play sound2
  wait 3 
  setPanning playing (mkPanning 1)
  wait 2 
  setPanning playing (mkPanning 0)
  wait 2
  mute playing
  unsafeEff_ $ putStrLn "muted"
  wait 2
  setVolume playing (mkVolume 1.0)
  unsafeEff_ $ putStrLn "before w"
  wait 2
  stopSound playing
  wait 2 
  unsafeEff_ $ putStrLn "after w"
  pure ()
  where 
    wait x = unsafeEff_ $ threadDelay (x * 1000000)
