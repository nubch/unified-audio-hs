{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module UnifiedAudio.Effectful where
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

--- Effectful

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

--- Smart Constructor

load :: Audio s :> es => FilePath -> Eff es (s Loaded)
load bytes = do
  AudioRep backend <- getStaticRep
  unsafeEff_ (loadA backend bytes)

resume :: Audio s :> es => s Paused -> Eff es (s Playing)
resume playing = do
  AudioRep backend <- getStaticRep
  unsafeEff_ (backend.resumeA playing)

-- something extra here, we can easily write functions that do not
-- correspond 1:1 to the backend functions
loadFile :: Audio s :> es => FilePath -> Eff es (s Loaded)
loadFile filePath =
  unsafeEff_ (readFile filePath) >>= load

play :: Audio s :> es => s Loaded -> Eff es (s Playing)
play sound = do
  AudioRep backend <- getStaticRep
  unsafeEff_ (playA backend sound)

pause :: Audio s :> es => s Playing -> Eff es (s Paused)
pause pl = do
  AudioRep backend <- getStaticRep
  unsafeEff_ (pauseA backend pl) 

--stopSound :: (AudioEffect playing :> es) => playing -> Eff es ()
--stopSound playing = do
  --AudioRep backend <- getStaticRep
  --unsafeEff_ $ backend.stopSoundB playing

--setVolume :: (AudioEffect playing :> es) => playing -> Volume -> Eff es ()
--setVolume playing volume = do
  --AudioRep backend <- getStaticRep
  --unsafeEff_ $ backend.setVolumeB playing volume

--setPanning :: (AudioEffect playing :> es) => playing -> Panning -> Eff es ()
--setPanning playing panning = do
  --AudioRep backend <- getStaticRep
  --unsafeEff_ $ backend.setPanningB playing panning

--- Utilities

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