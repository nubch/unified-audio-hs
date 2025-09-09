{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Effectful
import Effectful.Dispatch.Static ( unsafeEff_, unsafeEff )
import Control.Concurrent
import UnifiedAudio.Effectful
import qualified SDL.Backend as SDL
import qualified Fmod.Backend as Fmod
import qualified Data.ByteString as BS
import Data.Char (GeneralCategory(ModifierLetter))
import Fmod.Safe (stopChannel)

main :: IO ()
main = runEff $ Fmod.runAudio fileTypeTestBytes

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  
  bytes <- unsafeEff_ (BS.readFile "sounds/example.wav")
  wav <- loadBytes bytes Mono
  playing <- play wav (Times 1)
  wait 2
  liftIO $ putStrLn "Panning..."
  setPanning playing (mkPanning (0.0))
  wait 5000
  pure ()

  where
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
  




