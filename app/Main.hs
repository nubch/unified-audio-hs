{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Effectful
import Effectful.Dispatch.Static ( unsafeEff_, unsafeEff )
import Control.Concurrent ( threadDelay )
import UnifiedAudio.Effectful
import qualified SDL.Backend as SDL
import qualified Fmod.Backend as Fmod
import qualified Data.ByteString as BS
import Data.Char (GeneralCategory(ModifierLetter))
import Fmod.Safe (stopChannel, stopGroup)

main :: IO ()
main = runEff $ Fmod.runAudio groupTest

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  
  bytes <- unsafeEff_ (BS.readFile "sounds/example.wav")
  wav <- loadBytes bytes Mono
  playing <- play wav (Times 5)
  awaitFinished playing
  pure ()

  where
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

-- Simple demo to exercise group functionality in FMOD backend
groupTest :: (Audio channel :> es, IOE :> es) => Eff es ()
groupTest = do
  write "Loading two sounds"
  wavB <- unsafeEff_ (BS.readFile "sounds/example.wav")
  mp3B <- unsafeEff_ (BS.readFile "sounds/example.mp3")
  s1 <- loadBytes wavB Stereo
  s2 <- loadBytes mp3B Stereo

  write "Playing both (Forever)"
  move <- play s1 Forever
  flim <- play s2 Forever

  write "mkOrGetGroup: create or get named group 'SFX'"
  g1 <- mkOrGetGroup "SFX"
  addToGroup g1 move
  addToGroup g1 flim
  wait 3
  write "Pan group left, then right, then center"
  setGroupPanning g1 (mkPanning (-1))
  wait 1
  setGroupPanning g1 (mkPanning 1)
  wait 1
  setGroupPanning g1 (mkPanning 0)
  wait 1
  write "Set group volume to 0.3 (both attenuated)"
  setGroupVolume g1 (mkVolume 0.3)
  wait 2
  write "Restore group volume to 1.0"
  setGroupVolume g1 (mkVolume 1.0)
  wait 1
  write "mkOrGetGroup again for 'SFX' (same underlying group)"
  gAgain <- mkOrGetGroup "SFX"
  stop flim

  write "Pause group via second handle (both should pause)"
  pauseGroup gAgain
  wait 2

  write "Attempt resume move only while group paused (should remain effectively paused)"
  move' <- resume =<< pause move 
  let _ = move'
  wait 2

  write "Resume group (both should play)"
  resumeGroup g1
  wait 3

  write "Remove flim from group and pause group (only move should pause)"
  removeFromGroup g1 flim
  pauseGroup g1
  wait 1

  write "Create or get new named group 'SFX-2' and move flim there"
  wait 2
  g2 <- mkOrGetGroup "SFX-2"
  wait 2
  addToGroup g2 flim
  wait 2
  write "Pause 'SFX-2' (only flim should pause now)"
  wait 2
  pauseGroup g2
  resumeGroup g1
  wait 8
  write "Resume 'SFX-2'"
  resumeGroup g2
  wait 2

  write "Cleanup: stop both"
  _ <- stop move
  _ <- stop flim
  write "Group test complete"
  where
    write = liftIO . putStrLn
    wait x = unsafeEff_ $ threadDelay (x * 1000000)


fileTypeTestBytes :: (Audio channel :> es, IOE :> es) => Eff es ()
fileTypeTestBytes = do
  wavB <- unsafeEff_ (BS.readFile "sounds/example.wav") 
  mp3B <- unsafeEff_ (BS.readFile "sounds/example.mp3") 
  oggB <- unsafeEff_ (BS.readFile "sounds/example.ogg") 
  flacB <- unsafeEff_ (BS.readFile "sounds/example.flac")

  wav  <- loadBytes wavB Stereo
  mp3  <- loadBytes mp3B Stereo
  ogg  <- loadBytes oggB Stereo
  flac <- loadBytes flacB Stereo

  --- wav
  write "Playing WAV"
  wav' <- play wav Once
  pan <- getPanning wav'
  vol <- getVolume wav'
  write $ show pan
  write $ show vol
  ---
  setPanning wav' (mkPanning 0.7)
  setVolume wav' (mkVolume 0.7)
  pan <- getPanning wav'
  vol <- getVolume wav'
  write $ show pan
  write $ show vol
  wait 3
  _ <- stop wav'
  write "Stopped WAV"
  write ""

  --- mp3
  write "Playing MP3"
  mp3' <- play mp3 Once
  wait 5
  _ <- stop mp3'
  write "Stopped MP3"
  write ""

  --- ogg
  write "Playing OGG"
  ogg' <- play ogg Once
  wait 5
  _ <- stop ogg'
  write "Stopped OGG"
  write ""

  --- flac
  write "Playing FLAC"
  flac' <- play flac Once
  wait 5
  _ <- stop flac' 
  write "Stopped FLAC"
  write ""
  dd <- play mp3 Once
  write "Test successful"
  where
    write = liftIO . putStrLn
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

fileTypeTestFilePath :: (Audio channel :> es, IOE :> es) => Eff es ()
fileTypeTestFilePath = do
  wav  <- loadFile "sounds/example.wav" Stereo
  mp3  <- loadFile "sounds/example.mp3" Stereo
  ogg  <- loadFile "sounds/example.ogg" Stereo
  flac <- loadFile "sounds/example.flac" Stereo

  --- wav
  write "Playing WAV"
  wav' <- play wav Once
  wait 3
  _ <- stop wav'
  write "Stopped WAV"
  write ""

  --- mp3
  write "Playing MP3"
  mp3' <- play mp3 Once
  wait 5
  _ <- stop mp3'
  write "Stopped MP3"
  write ""

  --- ogg
  write "Playing OGG"
  ogg' <- play ogg Once
  wait 5
  _ <- stop ogg'
  write "Stopped OGG"
  write ""

  --- flac
  write "Playing FLAC"
  flac' <- play flac Once
  wait 5
  _ <- stop flac' 
  write "Stopped FLAC"
  write ""
  write "Test successful"
  where
    write = liftIO . putStrLn
    wait x = unsafeEff_ $ threadDelay (x * 1000000)
