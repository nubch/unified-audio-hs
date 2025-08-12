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
import Fmod.Safe (stopChannel)

main :: IO ()
main = runEff $ SDL.runAudio test

test :: (Audio channel :> es, IOE :> es) => Eff es ()
test = do
  sound2 <- load "playPiece.wav"
  playing2 <- play sound2 (Times 4)
  onFinished playing2 (\p -> liftIO $ putStrLn "Sound finished playing") 
  wait 10000
  wait 10
  pure ()
  where
    wait x = unsafeEff_ $ threadDelay (x * 1000000)

