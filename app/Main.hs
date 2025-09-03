{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Effectful
import Effectful.Dispatch.Static ( unsafeEff_, unsafeEff )
import Control.Concurrent (threadDelay)
import qualified UnifiedAudio.Mock as Mock
import Control.Concurrent
import qualified SDL.Backend as SDL
import qualified Fmod.Backend as Fmod
import UnifiedAudio.Effectful
--import Fmod.Safe (stopChannel)

main :: IO ()
main = runEff $ Fmod.runAudio test

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  sound2 <- load "playPiece.wav"
  music <- load "flim.mp3"
  once <- play sound2 Once
  mus <- play music Once
  wait 10
  fin <- hasFinished once
  fin2 <- hasFinished mus
  liftIO $ putStrLn $ "Has finished repeat? " ++ show fin
  liftIO $ putStrLn $ "Has finished once? " ++ show fin2
  pure ()
  where
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

