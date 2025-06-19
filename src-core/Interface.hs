{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interface
  ( AudioEffect,
    AudioBackend(..),
    StaticRep(..),
    Volume,
    mkVolume,
    unVolume
  ) where

import Data.Kind
import Effectful
import Effectful.Dispatch.Static

data AudioEffect (playing :: Type) :: Effect

newtype Volume = Volume Float deriving Show -- Only values between 0 and 1
  
type instance DispatchOf (AudioEffect playing) = Static WithSideEffects

newtype instance StaticRep (AudioEffect playing) = AudioRep (AudioBackend playing)

data AudioBackend playing = AudioBackend
  { 
    playSoundB :: FilePath -> IO playing,
    stopSoundB :: playing -> IO (),
    setVolumeB :: Volume -> playing -> IO ()
  }

mkVolume :: Float -> Volume
mkVolume vol = Volume (clamp vol)
  where clamp = max 0.0 . min 1.0

unVolume :: Volume -> Float
unVolume (Volume v) = v 