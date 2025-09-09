{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module UnifiedAudio.Effectful where

import Effectful
import Effectful.Dispatch.Static
import Data.Kind (Type)

import GHC.Exts (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified Data.ByteString as BS

data Source = FromFile FilePath | FromBytes BS.ByteString

data Status = Loaded | Playing | Paused | Stopped
  deriving (Show, Eq)

data SoundType = Mono | Stereo
  deriving (Show, Eq)

data Times = Once | Times Int | Forever
  deriving (Show, Eq)

--- Effectful

data Audio (s :: Status -> Type) :: Effect

data AudioBackend (s :: Status -> Type) = AudioBackend
  { 
    loadA         :: Source -> SoundType -> IO (s 'Loaded)
  , playA         :: s 'Loaded -> Times -> IO (s 'Playing)
  , pauseA        :: s 'Playing -> IO (s 'Paused)
  , resumeA       :: s 'Paused  -> IO (s 'Playing)

  , setVolumeA    :: forall st. Adjustable st => s st -> Volume  -> IO ()
  , setPanningA   :: forall st. Adjustable st => s st -> Panning -> IO ()
  , stopChannelA  :: forall st. Stoppable  st => s st -> IO (s 'Stopped)

  , hasFinishedA  :: s 'Playing -> IO Bool
  , unloadA       :: s 'Loaded  -> IO ()
  }

type instance DispatchOf (Audio s) = Static WithSideEffects
newtype instance StaticRep (Audio s) = AudioRep (AudioBackend s)

newtype Volume = Volume Float deriving (Show, Eq) -- Only values between 0 and 1

newtype Panning = Panning Float deriving (Show, Eq)

type family Stoppable (st :: Status) :: Constraint where
  Stoppable 'Playing = ()
  Stoppable 'Paused  = ()
  Stoppable other    =
    TypeError
      ( 'Text "operation requires a stoppable channel; got "
     ':<>: 'ShowType other )

type family Adjustable (st :: Status) :: Constraint where
  Adjustable 'Playing = ()
  Adjustable 'Paused  = ()
  Adjustable other    =
    TypeError
      ( 'Text "operation requires an adjustable channel; got "
     ':<>: 'ShowType other )

--- Smart Constructors
load :: Audio s :> es => Source -> SoundType -> Eff es (s Loaded)
load src stype = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.loadA src stype

loadFile :: Audio s :> es => FilePath -> SoundType -> Eff es (s Loaded)
loadFile fp = load (FromFile fp)

loadBytes :: Audio s :> es => BS.ByteString -> SoundType -> Eff es (s Loaded)
loadBytes bs = load (FromBytes bs)

unload :: Audio s :> es => s Loaded -> Eff es ()
unload sound = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.unloadA sound

resume :: Audio s :> es => s Paused -> Eff es (s Playing)
resume channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.resumeA channel

play :: Audio s :> es => s Loaded -> Times -> Eff es (s Playing)
play sound times = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.playA sound times

pause :: Audio s :> es => s Playing -> Eff es (s Paused)
pause channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.pauseA channel

stop :: (Audio s :> es, Stoppable st) => s st -> Eff es (s 'Stopped)
stop channel = do
  AudioRep AudioBackend{ stopChannelA = stop' } <- getStaticRep
  unsafeEff_ (stop' channel)

setVolume :: (Audio s :> es, Adjustable st) => s st -> Volume -> Eff es ()
setVolume channel volume = do
  AudioRep AudioBackend{ setVolumeA = setVol } <- getStaticRep
  unsafeEff_ (setVol channel volume)

mute :: (Audio s :> es, Adjustable st) => s st -> Eff es ()
mute channel = setVolume channel (mkVolume 0.0)

setPanning :: (Audio s :> es, Adjustable st) => s st -> Panning -> Eff es ()
setPanning channel panning = do
  AudioRep AudioBackend{ setPanningA = setPan } <- getStaticRep
  unsafeEff_ (setPan channel panning)

hasFinished :: Audio s :> es => s Playing -> Eff es Bool
hasFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.hasFinishedA channel

--updateSystem :: Audio s :> es => s Loaded -> Eff es ()
--updateSystem loaded = do 
  --AudioRep backend <- getStaticRep
  --unsafeEff_ $ backend.updateSystemA loaded -- Placeholder for backends that need updating

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
