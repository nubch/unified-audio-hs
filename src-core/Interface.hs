{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module Interface where
--module Interface
  --( Audio,
    --AudioRep,
    --AudioBackend (..),
    --Volume,
    --mkVolume,
    --unVolume,
    --Panning,
    --mkPanning,
    --unPanning
  --) where

import Effectful
import Effectful.Dispatch.Static
import Data.Kind (Type)

data Status = Loaded | Playing | Paused

data Audio (s :: Status -> Type) :: Effect

data AudioBackend (s :: Status -> Type) = AudioBackend
  { 
    loadA :: FilePath -> IO (s Loaded),
    playA :: s Loaded -> IO (s Playing),
    pauseA :: s Playing -> IO (s Paused),
    resumeA :: s Paused -> IO (s Playing)
    --setVolumeA :: Volume -> s Playing -> IO (),
    --unloadA :: s Loaded -> IO ()
    -- and other operations, like seekA, loopA, or what have you
  }

type instance DispatchOf (Audio s) = Static WithSideEffects
newtype instance StaticRep (Audio s) = AudioRep (AudioBackend s)

newtype Volume = Volume Float deriving Show -- Only values between 0 and 1

newtype Panning = Panning Float deriving Show

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