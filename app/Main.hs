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
import qualified UnifiedAudio.Mock as Mock
import Control.Concurrent
import qualified SDL.Backend as SDL
import qualified Fmod.Backend as Fmod
import UnifiedAudio.Effectful
import Fmod.Safe (stopChannel)
import Text.ParserCombinators.ReadPrec (lift)
--import Fmod.Safe (stopChannel)

main :: IO ()
main = runEff $ SDL.runAudio test

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  wav <- load "sounds/example.wav"
  playing <- play wav Once
  wait 3
  paused <- pause playing
  stopped <- stop paused
  setPanning stopped (Panning 0.5)
  setVolume paused (Volume 0.2)
  wait 2
  resumed <- resume paused
  wait 3
  _ <- stop resumed
  pure ()

  where
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

fileTypeTest :: (Audio channel :> es, IOE :> es) => Eff es ()
fileTypeTest = do
  wav  <- load "sounds/example.wav"
  mp3  <- load "sounds/example.mp3"
  ogg  <- load "sounds/example.ogg"
  flac <- load "sounds/example.flac"

  --- wav
  write "Playing WAV"
  wav' <- play wav Once
  wait 3
  _ <- stop wav'
  write "Stopped WAV"

  --- mp3
  write "Playing MP3"
  mp3' <- play mp3 Once
  wait 5
  _ <- stop mp3'
  write "Stopped MP3"

  --- ogg
  write "Playing OGG"
  ogg' <- play ogg Once
  wait 5
  _ <- stop ogg'
  write "Stopped OGG"

  --- flac
  write "Playing FLAC"
  flac' <- play flac Once
  wait 5
  _ <- stop flac' 
  write "Stopped FLAC"

  pure ()
  where
    write = liftIO . putStrLn
    wait x = unsafeEff_ $ threadDelay (x * 1000000)
  




