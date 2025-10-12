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
  describe "TP1: Audio formats load and play" $ do
    it "runs on FMOD and SDL" $ do
      runEff $ Fmod.runAudio TP.tp1
      runEff $ SDL.runAudio TP.tp1

  describe "TP2: Volume clamping" $ do
    it "clamps volume on both backends" $ do
      runEff $ Fmod.runAudio TP.tp2
      runEff $ SDL.runAudio TP.tp2

  describe "TP3: Panning clamping" $ do
    it "clamps panning on both backends" $ do
      runEff $ Fmod.runAudio TP.tp3
      runEff $ SDL.runAudio TP.tp3

  describe "TP4: Legal state transitions" $ do
    it "accepts play->pause->resume->stop" $ do
      runEff $ Fmod.runAudio TP.tp4
      runEff $ SDL.runAudio TP.tp4

  describe "TP5: Looping and hasFinished" $ do
    it "validates loop modes and monotonic hasFinished" $ do
      runEff $ Fmod.runAudio TP.tp5
      runEff $ SDL.runAudio TP.tp5

  describe "TP6: Group membership exclusivity" $ do
    it "moves channels between groups on both backends" $ do
      runEff $ Fmod.runAudio TP.tp6
      runEff $ SDL.runAudio TP.tp6

  describe "TP7: Group demo and adjustments" $ do
    it "runs on FMOD and SDL" $ do
      runEff $ Fmod.runAudio TP.tp7
      runEff $ SDL.runAudio TP.tp7