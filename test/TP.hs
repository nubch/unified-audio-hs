{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TP where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad (void)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_, unEff)
import Fmod.Backend qualified as Fmod
import Test.Hspec (shouldBe)
import UnifiedAudio.Effectful

wait :: Int -> Eff es ()
wait x = unsafeEff_ $ threadDelay (x * 1000000)

write :: (IOE :> es) => String -> Eff es ()
write = liftIO . putStrLn

tp1 :: (Audio s :> es, IOE :> es) => Eff es ()
tp1 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo
  mp3 <- loadFile "sounds/exampleFile.mp3" Stereo
  ogg <- loadFile "sounds/exampleFile.ogg" Stereo
  flac <- loadFile "sounds/exampleFile.flac" Stereo

  write "Playing WAV"
  wav' <- play wav Once
  wait 2
  awaitFinished wav'
  _ <- stop wav'

  write "Playing MP3"
  mp3' <- play mp3 Once
  wait 2
  awaitFinished mp3'
  _ <- stop mp3'

  write "Playing OGG"
  ogg' <- play ogg Once
  wait 2
  _ <- stop ogg'
  awaitFinished ogg'

  write "Playing FLAC"
  flac' <- play flac Once
  wait 2
  awaitFinished flac'
  write "Unloading all files"
  mapM_ unload [wav, mp3, ogg, flac]
  

tp2 :: (Audio s :> es, IOE :> es) => Eff es ()
tp2 = do
  let vols = [-10, -0.1, 0, 0.5, 1, 1.5, 9999]
      pans = [5, -1, 0, 1, -5]
  s <- loadFile "sounds/exampleFile.wav" Mono
  chM <- play s Forever

  -- Volume: set, read, and check each value in sequence
  mapM_ (\v -> do
            setVolume chM (mkVolume v)
            observed <- getVolume chM
            liftIO $ observed `shouldBe` mkVolume v
        ) vols

  -- Panning: set, read, and check each value in sequence
  mapM_ (\x -> do
            setPanning chM (mkPanning x)
            p <- getPanning chM
            liftIO $ p `shouldBe` mkPanning x
        ) pans

  void (stop chM)

tp3 :: (Audio s :> es, IOE :> es) => Eff es ()
tp3 = do
  l <- loadFile "sounds/exampleFile.wav" Stereo
  p <- play l Once
  paused <- pause p
  r <- resume paused
  _ <- stop r
  pure ()

-- TP5: Looping and hasFinished monotonicity (AC5, AC6)
tp4 :: (Audio s :> es, IOE :> es) => Eff es ()
tp4 = do
  -- once: eventually becomes finished
  onceS <- loadFile "sounds/exampleFile.wav" Stereo
  write "Sound should play once"
  ch1 <- play onceS Once
  f0 <- hasFinished ch1
  liftIO $ f0 `shouldBe` False
  awaitFinished ch1
  p1 <- hasFinished ch1
  liftIO $ p1 `shouldBe` True

  -- times n
  tS <- loadFile "sounds/exampleFile.wav" Stereo
  ch2 <- play tS (Times 2)
  write "Sound should play twice"
  awaitFinished ch2
  p2 <- hasFinished ch2
  liftIO $ p2 `shouldBe` True

  -- forever then stop
  fS <- loadFile "sounds/exampleFile.wav" Stereo
  write "Sound should loop indefinitely"
  chF <- play fS Forever
  wait 3
  f3 <- hasFinished chF
  liftIO $ f3 `shouldBe` False
  void (stop chF)


-- TP6: Group membership exclusivity (AC7)
tp5 :: (Audio s :> es, IOE :> es) => Eff es ()
tp5 = do
  s <- loadFile "sounds/exampleFile.wav" Stereo
  ch <- play s Forever
  g1 <- makeGroup
  g2 <- makeGroup
  addToGroup g1 ch
  -- moving to g2 must remove from g1
  addToGroup g2 ch
  -- pausing g1 should not affect ch anymore
  pauseGroup g1
  write "Sound should still be playing"
  wait 5 
  void (stop ch)

-- Add more:
-- it "TP3: Looping" $ runWithBackend tp3

-- TP7: Group demo exercising group bus behavior consistently across backends
tp6 :: (Audio s :> es, IOE :> es) => Eff es ()
tp6 = do
  wavSound <- loadFile "sounds/example.wav" Stereo
  mp3Sound <- loadFile "sounds/example.mp3" Stereo

  wavChannel <- play wavSound Forever
  mp3Channel <- play mp3Sound Forever

  group1 <- makeGroup
  addToGroup group1 wavChannel
  addToGroup group1 mp3Channel

  wait 1
  write "Pan group left, then right, then center"
  setGroupPanning group1 (mkPanning (-1))
  wait 1
  setGroupPanning group1 (mkPanning 1)
  wait 1
  setGroupPanning group1 (mkPanning 0)
  wait 1

  write "Set group volume to 0.3 (both attenuated)"
  setGroupVolume group1 (mkVolume 0.3)
  wait 1
  write "Restore group volume to 1.0"
  setGroupVolume group1 (mkVolume 1.0)
  wait 1
  write "Pause group via handle (both should pause)"
  pauseGroup group1
  wait 1

  write "Resume group (both should play)"
  resumeGroup group1
  wait 2

  write "Cleanup: stop both"
  write "Stopping via group handle"
  stopGroup group1
  -- Optionally, also ensure individual stops are harmless no-ops
  _ <- stop wavChannel
  void $ stop mp3Channel

-- Resuming a channel while its group is paused keeps it effectively paused.
-- After resuming the group, the channel proceeds to finish.
tp8 :: (Audio s :> es, IOE :> es) => Eff es ()
tp8 = do
  sound <- loadFile "sounds/exampleFile.wav" Stereo
  channel <- play sound Once
  group <- makeGroup
  addToGroup group channel
  pauseGroup group
  write "Track should be paused"
  channelAfterResume <- resume =<< pause channel
  wait 5 
  resumeGroup group
  write "Track should be playing"
  awaitFinished channelAfterResume
  pure ()
