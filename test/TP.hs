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
  wav <- loadFile "sounds/exampleFile.wav" Mono
  wav' <- play wav Once

  awaitFinished wav'

  void $ stop wav'
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
  void $ unload wav

tp3 :: (Audio s :> es, IOE :> es) => Eff es ()
tp3 = do
  wav <- loadFile "sounds/exampleFile.wav" Mono
  wav' <- play wav Once

  paused <- pause wav'
  resumed <- resume paused

  void $ stop resumed
  void $ unload wav

tp4 :: (Audio s :> es, IOE :> es) => Eff es ()
tp4 = do
  wav <- loadFile "sounds/exampleFile.wav" Mono
  wav' <- play wav Forever

  wait 3
  paused <- pause wav'
  wait 1
  resumed <- resume paused

  void $ stop resumed
  void $ unload wav


tp5 :: (Audio s :> es, IOE :> es) => Eff es ()
tp5 = do
  wav <- loadFile "sounds/exampleFile.wav" Mono
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
  void $ unload wav

tp6 :: (Audio s :> es, IOE :> es) => Eff es ()
tp6 = do
  wav <- loadFile "sounds/exampleFile.wav" Mono
  wav' <- loadFile "sounds/exampleFile2.wav" Mono

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

  void $ unload wav
  void $ unload wav'