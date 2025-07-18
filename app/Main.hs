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
import Audio 
import qualified Mock
import Control.Concurrent
import qualified SDL.SDL as SDL
import qualified Fmod.Fmod as Fmod
import qualified Interface as I

main :: IO ()
main = runEff $ Fmod.runAudio test

test :: (I.Audio channel :> es) => Eff es ()
test = do
  sound <- load "flim.mp3"
  playing <- play sound
  wait 5 
  paused <- pause playing
  wait 5 
  _ <- resume paused
  wait 5 
  pure ()
  where 
    wait x = unsafeEff_ $ threadDelay (x * 1000000)
