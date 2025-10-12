{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TP where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_, unEff)
import Fmod.Backend qualified as Fmod
import Test.Hspec (shouldBe)
import UnifiedAudio.Effectful
import Control.Arrow (ArrowLoop(loop))

wait :: Int -> Eff es ()
wait x = unsafeEff_ $ threadDelay (x * 1000000)

write :: (IOE :> es) => String -> Eff es ()
write = liftIO . putStrLn

tp1 :: (Audio s :> es, IOE :> es) => Eff es ()
tp1 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo

  write "Playing WAV"
  wav' <- play wav Once
  awaitFinished wav'
  _ <- stop wav'

  void $ unload wav
  

tp2 :: (Audio s :> es, IOE :> es) => Eff es ()
tp2 = do
  let vols = [-10, -0.1, 0, 0.5, 1, 1.5, 9999]
      pans = [5, -1, 0, 1, -5]
  wav <- loadFile "sounds/exampleFile.wav" Mono
  wav' <- play wav Forever

  mapM_ (\v -> do
            setVolume wav' (mkVolume v)
            observed <- getVolume wav'
            liftIO $ observed `shouldBe` mkVolume v
        ) vols

  mapM_ (\x -> do
            setPlacement wav' (mkPlacement x)
            p <- getPlacement wav'
            liftIO $ p `shouldBe` mkPlacement x
        ) pans

  void $ stop wav'

tp3 :: (Audio s :> es, IOE :> es) => Eff es ()
tp3 = do
  l <- loadFile "sounds/exampleFile.wav" Stereo
  p <- play l Once
  paused <- pause p
  r <- resume paused
  void $ stop r

-- TP5: Looping and hasFinished monotonicity (AC5, AC6)
tp4 :: (Audio s :> es, IOE :> es) => Eff es ()
tp4 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo

  -- once: eventually becomes finished
  write "Sound should play once"
  once <- play wav Once
  f1 <- hasFinished once
  liftIO $ f1 `shouldBe` False
  awaitFinished once
  f2 <- hasFinished once
  liftIO $ f2 `shouldBe` True

  -- forever then stop
  write "Sound should loop indefinitely"
  looping <- play wav Forever
  wait 3
  f3 <- hasFinished looping
  liftIO $ f3 `shouldBe` False
  stop looping
  f4 <- hasFinished looping
  liftIO $ f4 `shouldBe` True


-- TP6: Group membership exclusivity (AC7)
tp5 :: (Audio s :> es, IOE :> es) => Eff es ()
tp5 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo
  looping <- play wav Forever
  g1 <- makeGroup
  g2 <- makeGroup
  addToGroup g1 looping
  -- moving to g2 must remove from g1
  addToGroup g2 looping
  -- pausing g1 should not affect ch anymore
  pauseGroup g1
  write "Sound should still be playing"
  wait 5 
  void $ stop looping

-- TP7: Group demo exercising group bus behavior consistently across backends
tp6 :: (Audio s :> es, IOE :> es) => Eff es ()
tp6 = do
  wav <- loadFile "sounds/example.wav" Stereo
  wav' <- loadFile "sounds/exampleFile.wav" Stereo

  ch1 <- play wav Forever
  ch2 <- play wav' Forever

  group1 <- makeGroup
  addToGroup group1 ch1
  addToGroup group1 ch2

  wait 1
  write "Pan group left, then right, then center"
  setGroupPlacement group1 (mkPlacement (-1))
  wait 1
  setGroupPlacement group1 (mkPlacement 1)
  wait 1
  setGroupPlacement group1 (mkPlacement 0)
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
  -- Optional, but ensures individual stops are harmless
  void $ stop ch1
  void $ stop ch2

-- Resuming a channel while its group is paused keeps it effectively paused.
-- After resuming the group, the channel proceeds to finish.
tp7 :: (Audio s :> es, IOE :> es) => Eff es ()
tp7 = do
  wav <- loadFile "sounds/exampleFile.wav" Stereo
  wav' <- play wav Once
  group <- makeGroup
  addToGroup group wav'
  pauseGroup group
  write "Track should be paused"
  channelAfterResume <- resume =<< pause wav'
  wait 2 
  resumeGroup group
  write "Track should be playing"
  awaitFinished channelAfterResume
