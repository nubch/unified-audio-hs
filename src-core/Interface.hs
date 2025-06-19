{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Interface
  ( AudioEffect,
    AudioBackend(..),
    StaticRep(..),
    Volume,
    mkVolume,
    unVolume,
    Panning,
    mkPanning,
    unPanning
  ) where

import Data.Kind
import Effectful
import Effectful.Dispatch.Static

data AudioEffect (playing :: Type) :: Effect

newtype Volume = Volume Float deriving Show -- Only values between 0 and 1

newtype Panning = Panning Float deriving Show
  
type instance DispatchOf (AudioEffect playing) = Static WithSideEffects

newtype instance StaticRep (AudioEffect playing) = AudioRep (AudioBackend playing)

data AudioBackend playing = AudioBackend
  { 
    playSoundB  :: FilePath -> IO playing,
    stopSoundB  :: playing -> IO (),
    setVolumeB  :: playing -> Volume -> IO (),
    setPanningB :: playing -> Panning -> IO () 
  }

mkPanning :: Float -> Panning
mkPanning x = Panning (clamp x)
  where clamp = max (-1.0) . min 1.0

unPanning :: Panning -> Float
unPanning (Panning x) = x

mkVolume :: Float -> Volume
mkVolume x = Volume (clamp x)
  where clamp = max 0.0 . min 1.0

unVolume :: Volume -> Float
unVolume (Volume v) = v 