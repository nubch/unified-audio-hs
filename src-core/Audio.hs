{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Audio
  ( playSound
  , stopSound
  , setVolume
  , setPanning
  ) where

import Effectful
import Effectful.Dispatch.Static
import Interface

playSound :: (AudioEffect playing :> es) => FilePath -> Eff es playing
playSound fp = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.playSoundB fp

stopSound :: (AudioEffect playing :> es) => playing -> Eff es ()
stopSound playing = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.stopSoundB playing

setVolume :: (AudioEffect playing :> es) => playing -> Volume -> Eff es ()
setVolume playing volume = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.setVolumeB playing volume

setPanning :: (AudioEffect playing :> es) => playing -> Panning -> Eff es ()
setPanning playing panning = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.setPanningB playing panning
