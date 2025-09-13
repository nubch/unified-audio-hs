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
import Fmod.Safe (stopChannel)

main :: IO ()
main = runEff $ Fmod.runAudio groupTest

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  
  bytes <- unsafeEff_ (BS.readFile "sounds/example.wav")
  wav <- loadBytes bytes Mono
  playing <- play wav (Times 1)
  g <- mkGroup "music"
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
  c1 <- play s1 Forever
  c2 <- play s2 Forever

  write "Creating group G and adding channels"
  g <- mkGroup "G"
  addToGroup g c1
  addToGroup g c2
  wait 5
  write "Pause group (both should pause)"
  pauseGroup g
  wait 2

  write "Attempt resume c1 only while group paused (should remain effectively paused)"
  c1' <- resume =<< pause c1 
  let _ = c1'
  wait 2

  write "Resume group (both should play)"
  resumeGroup g
  wait 3

  write "Remove c2 from group and pause group (only c1 should pause)"
  removeFromGroup g c2
  pauseGroup g
  wait 2

  write "Cleanup: stop both"
  _ <- stop c1
  _ <- stop c2
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
