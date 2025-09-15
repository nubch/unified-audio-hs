{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TP where

import Control.Concurrent (threadDelay)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Fmod.Backend qualified as Fmod
import Test.Hspec (shouldBe)
import UnifiedAudio.Effectful

tp1 :: (Audio s :> es, IOE :> es) => Eff es ()
tp1 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo
  mp3 <- loadFile "sounds/exampleFile.mp3" Stereo
  ogg <- loadFile "sounds/exampleFile.ogg" Stereo
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
  wavSound <- loadFile "sounds/example.wav" Stereo
  mp3Sound <- loadFile "sounds/example.mp3" Stereo

  write "Playing both (Forever)"
  wavChannel <- play wavSound Forever
  mp3Channel <- play mp3Sound Forever

  write "mkOrGetGroup: create or get named group 'SFX'"
  groupSFX <- mkOrGetGroup "SFX"
  addToGroup groupSFX wavChannel
  addToGroup groupSFX mp3Channel
  wait 1
  write "Pan group left, then right, then center"
  setGroupPanning groupSFX (mkPanning (-1))
  wait 1
  setGroupPanning groupSFX (mkPanning 1)
  wait 1
  setGroupPanning groupSFX (mkPanning 0)
  wait 1
  write "Set group volume to 0.3 (both attenuated)"
  setGroupVolume groupSFX (mkVolume 0.3)
  wait 1
  write "Restore group volume to 1.0"
  setGroupVolume groupSFX (mkVolume 1.0)
  wait 1
  write "Pause group via handle (both should pause)"
  pauseGroup groupSFX
  wait 1

  write "Attempt resume of one channel only while group paused (should remain paused)"
  _ <- resume =<< pause wavChannel
  wait 1

  write "Resume group (both should play)"
  resumeGroup groupSFX
  wait 2

  write "Cleanup: stop both"
  _ <- stop wavChannel
  _ <- stop mp3Channel
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
  sound <- loadFile "sounds/exampleFile.wav" Stereo
  channel <- play sound Forever
  let baseVol = mkVolume 0.8
      basePan = mkPanning 0.2
  setVolume channel baseVol
  setPanning channel basePan

  group <- mkOrGetGroup "TP8-Group"
  addToGroup group channel
  setGroupVolume group (mkVolume 0.5)
  setGroupPanning group (mkPanning (-0.5))

  observedVolume <- getVolume channel
  observedPanning <- getPanning channel
  liftIO $ observedVolume `shouldBe` baseVol
  liftIO $ observedPanning `shouldBe` basePan

  _ <- stop channel
  pure ()

-- TP9: Group pause uses OR semantics with channel pause
-- Expectations:
-- - Resuming a channel while its group is paused keeps it effectively paused.
-- - After resuming the group, the channel proceeds to finish.
tp9 :: (Audio s :> es, IOE :> es) => Eff es ()
tp9 = do
  sound <- loadFile "sounds/exampleFile.wav" Stereo
  channel <- play sound Once
  group <- mkOrGetGroup "TP9-Group"
  addToGroup group channel
  pauseGroup group
  channelAfterResume <- resume =<< pause channel
  unsafeEff_ $ threadDelay 500000
  finishedBeforeGroupResume <- hasFinished channelAfterResume
  liftIO $ finishedBeforeGroupResume `shouldBe` False
  resumeGroup group
  awaitFinished channelAfterResume
  pure ()

-- TP10: Group panning vs individual channel panning
-- Expectations:
-- - Setting group panning does not change channel-level getters.
-- - Changing a channel's pan while group pan is active reflects in getters.
tp10 :: (Audio s :> es, IOE :> es) => Eff es ()
tp10 = do
  wavSound <- loadFile "sounds/exampleFile.wav" Stereo
  mp3Sound <- loadFile "sounds/exampleFile.mp3" Stereo

  wavChannel <- play wavSound Forever
  mp3Channel <- play mp3Sound Forever

  let basePanWav = mkPanning (-0.3)
      basePanMp3 = mkPanning 0.4
  setPanning wavChannel basePanWav
  setPanning mp3Channel basePanMp3

  group <- mkOrGetGroup "TP10-Group"
  addToGroup group wavChannel
  addToGroup group mp3Channel

  -- apply group pan
  setGroupPanning group (mkPanning 0.5)

  -- getters should reflect base, not group
  observedPanWav <- getPanning wavChannel
  observedPanMp3 <- getPanning mp3Channel
  liftIO $ observedPanWav `shouldBe` basePanWav
  liftIO $ observedPanMp3 `shouldBe` basePanMp3

  -- change an individual pan while group pan is active
  let updatedPanMp3 = mkPanning (-0.2)
  setPanning mp3Channel updatedPanMp3
  observedPanMp3Updated <- getPanning mp3Channel
  liftIO $ observedPanMp3Updated `shouldBe` updatedPanMp3

  -- cleanup
  _ <- stop wavChannel
  _ <- stop mp3Channel
  pure ()
