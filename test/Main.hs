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




