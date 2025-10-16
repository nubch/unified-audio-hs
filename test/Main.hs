{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Effectful
import Effectful.Dispatch.Static (unsafeEff_) 
import UnifiedAudio.Effectful
import Control.Concurrent ( threadDelay )
import qualified SDL.Backend as SDL
import Test.Hspec
import qualified TP 
import qualified Fmod.Backend as Fmod

main :: IO ()
main = hspec $ do
  describe "TP1 - Sound loading and Stop/Finish Behavior" $ do
    it "The file loads and unloads correctly and playback starts. awaitFinished works as intended" $ do
      runEff $ Fmod.runAudio TP.tp1
      runEff $ SDL.runAudio TP.tp1

  describe "TP2 - Volume and Stereo Placement" $ do
    it "Observed values equal the expected normalized inputs for all test values." $ do
      runEff $ Fmod.runAudio TP.tp2
      runEff $ SDL.runAudio TP.tp2

  describe "TP3 - Legal State Transitions" $ do
    it "Each operation succeeds without error." $ do
      runEff $ Fmod.runAudio TP.tp3
      runEff $ SDL.runAudio TP.tp3

  describe "TP4 - Loop Interrupt" $ do
    it "Pausing and resuming the channel is successful" $ do
      runEff $ Fmod.runAudio TP.tp4
      runEff $ SDL.runAudio TP.tp4

  describe "TP5 - Moving Channels to a Group" $ do
    it "Pausing group1 no longer affects the channel after it has been moved to group2 (channel keeps playing)." $ do
      runEff $ Fmod.runAudio TP.tp5
      runEff $ SDL.runAudio TP.tp5

  describe "TP6 - Group Adjustments and State Transitions" $ do
    it "Group operations affect both members as specified. State transitions propagate to the channels as expected." $ do
      runEff $ Fmod.runAudio TP.tp6
      runEff $ SDL.runAudio TP.tp6