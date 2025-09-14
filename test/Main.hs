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
  describe "Test Protocols (functions)" $ do
    it "TP1: Load & Play" $ do
      runEff $ Fmod.runAudio TP.tp1
      runEff $ SDL.runAudio TP.tp1

  describe "TP7: Group Demo" $ do
    it "runs on FMOD and SDL" $ do
      runEff $ Fmod.runAudio TP.tp7
      runEff $ SDL.runAudio TP.tp7

  describe "TP8: Equal panning/volume semantics" $ do
    it "returns base values on both backends" $ do
      runEff $ Fmod.runAudio TP.tp8
      runEff $ SDL.runAudio TP.tp8

  describe "TP9: Group Pause OR Semantics" $ do
    it "respects channel||group on both backends" $ do
      runEff $ Fmod.runAudio TP.tp9
      runEff $ SDL.runAudio TP.tp9

