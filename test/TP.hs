{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module TP where

import qualified Fmod.Backend as Fmod

import Effectful
import Effectful.Dispatch.Static (unsafeEff_) 
import UnifiedAudio.Effectful
import Control.Concurrent ( threadDelay )
import Test.Hspec (shouldBe)

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

-- TP7: Group demo exercising group bus behavior consistently across backends
tp7 :: (Audio s :> es, IOE :> es) => Eff es ()
tp7 = do
  write "Loading two sounds"
  s1 <- loadFile "sounds/example.wav" Stereo
  s2 <- loadFile "sounds/example.mp3" Stereo

  write "Playing both (Forever)"
  move <- play s1 Forever
  flim <- play s2 Forever

  write "mkOrGetGroup: create or get named group 'SFX'"
  g1 <- mkOrGetGroup "SFX"
  addToGroup g1 move
  addToGroup g1 flim
  wait 1
  write "Pan group left, then right, then center"
  setGroupPanning g1 (mkPanning (-1))
  wait 1
  setGroupPanning g1 (mkPanning 1)
  wait 1
  setGroupPanning g1 (mkPanning 0)
  wait 1
  write "Set group volume to 0.3 (both attenuated)"
  setGroupVolume g1 (mkVolume 0.3)
  wait 1
  write "Restore group volume to 1.0"
  setGroupVolume g1 (mkVolume 1.0)
  wait 1
  write "Pause group via handle (both should pause)"
  pauseGroup g1
  wait 1

  write "Attempt resume of one channel only while group paused (should remain paused)"
  _ <- resume =<< pause move
  wait 1

  write "Resume group (both should play)"
  resumeGroup g1
  wait 2

  write "Cleanup: stop both"
  _ <- stop move
  _ <- stop flim
  write "TP7 done"
  where
    write = liftIO . putStrLn
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

-- TP8: Equal panning/volume semantics across backends
-- Expectations:
-- - Channel getters return base values even after group volume/pan changes.
-- - Base channel setters preserve those base values while effective output reflects group.
tp8 :: (Audio s :> es, IOE :> es) => Eff es ()
tp8 = do
  snd <- loadFile "sounds/exampleFile.wav" Stereo
  ch  <- play snd Forever
  let baseVol = mkVolume 0.8
      basePan = mkPanning 0.2
  setVolume ch baseVol
  setPanning ch basePan

  g <- mkOrGetGroup "TP8-Group"
  addToGroup g ch
  setGroupVolume g (mkVolume 0.5)
  setGroupPanning g (mkPanning (-0.5))

  gv <- getVolume ch
  gp <- getPanning ch
  liftIO $ gv `shouldBe` baseVol
  liftIO $ gp `shouldBe` basePan

  _ <- stop ch
  pure ()

-- TP9: Group pause uses OR semantics with channel pause
-- Expectations:
-- - Resuming a channel while its group is paused keeps it effectively paused.
-- - After resuming the group, the channel proceeds to finish.
tp9 :: (Audio s :> es, IOE :> es) => Eff es ()
tp9 = do
  snd <- loadFile "sounds/exampleFile.wav" Stereo
  ch  <- play snd Once
  g   <- mkOrGetGroup "TP9-Group"
  addToGroup g ch
  pauseGroup g
  ch2 <- resume =<< pause ch
  unsafeEff_ $ threadDelay 500000
  still <- hasFinished ch2
  liftIO $ still `shouldBe` False
  resumeGroup g
  awaitFinished ch2
  pure ()
