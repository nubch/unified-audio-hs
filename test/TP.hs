{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module TP where

import qualified Fmod.Backend as Fmod

import Effectful
import Effectful.Dispatch.Static (unsafeEff_) 
import UnifiedAudio.Effectful
import Control.Concurrent ( threadDelay )

tp1 :: (Audio s :> es, IOE :> es) => Eff es ()
tp1 = do 
    wav  <- loadFile "sounds/exampleFile.wav" Stereo
    mp3  <- loadFile "sounds/exampleFile.mp3" Stereo
    ogg  <- loadFile "sounds/exampleFile.ogg" Stereo
    flac <- loadFile "sounds/exampleFile.flac" Stereo

    --- wav
    write "Playing WAV"
    wav' <- play wav Once
    wait 2
    _ <- stop wav'
    write "Stopped WAV"
    write ""

    --- mp3
    write "Playing MP3"
    mp3' <- play mp3 Once
    wait 2
    _ <- stop mp3'
    write "Stopped MP3"
    write ""

    --- ogg
    write "Playing OGG"
    ogg' <- play ogg Once
    wait 2
    _ <- stop ogg'
    write "Stopped OGG"
    write ""

    --- flac
    write "Playing FLAC"
    flac' <- play flac Once
    wait 2
    _ <- stop flac' 
    write "Stopped FLAC"
    write ""
    write "Test successful"
    where
        write = liftIO . putStrLn
        wait x = unsafeEff_ $ threadDelay (x * 1000000)
    -- Add more:
    -- it "TP3: Looping" $ runWithBackend tp3