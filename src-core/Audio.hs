{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Audio
  ( playSound
  , stopSound
  , setVolume
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

setVolume :: (AudioEffect playing :> es) => Volume -> playing -> Eff es ()
setVolume volume playing = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.setVolumeB volume playing
