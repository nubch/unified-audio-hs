{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}

module UnifiedAudio.Effectful where

import Effectful
import Effectful.Dispatch.Static
import Data.Kind (Type)

import GHC.Exts (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified Data.ByteString as BS

data Source = FromFile FilePath | FromBytes BS.ByteString

newtype Group (s :: Status -> Type) = GroupId Int 

data Status = Loaded | Playing | Paused | Stopped | Unloaded
  deriving (Show, Eq)

data SoundType = Mono | Stereo
  deriving (Show, Eq)

data Times = Once | Times Int | Forever
  deriving (Show, Eq)

--- Effectful

data Audio (s :: Status -> Type) :: Effect

data AudioBackend (s :: Status -> Type) = AudioBackend
  { 
    loadA          :: Source -> SoundType -> IO (s 'Loaded)
  , playA          :: s 'Loaded -> Times -> IO (s 'Playing)
  , pauseA         :: s 'Playing -> IO (s 'Paused)
  , resumeA        :: s 'Paused  -> IO (s 'Playing)

  , setVolumeA     :: forall st. Adjustable st => s st -> Volume  -> IO ()
  , getVolumeA     :: forall ad. Adjustable ad => s ad -> IO Volume
  , setPanningA    :: forall st. Adjustable st => s st -> Panning -> IO ()
  , getPanningA    :: forall ad. Adjustable ad => s ad -> IO Panning
  , stopChannelA   :: forall st. Stoppable  st => s st -> IO (s 'Stopped)

  , hasFinishedA   :: s 'Playing -> IO Bool
  , awaitFinishedA :: s 'Playing -> IO ()
  , unloadA        :: s 'Loaded  -> IO (s 'Unloaded)
  , mkGroupA       :: String        -> IO (Group s)
  , addToGroupA    :: forall st. Groupable st => Group s -> s st -> IO ()
  , removeFromGroupA :: forall st. Groupable st => Group s -> s st -> IO ()
  , pauseGroupA    :: Group s -> IO ()
  , resumeGroupA   :: Group s -> IO ()
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

-- Channels eligible for group membership/ops
type family Groupable (st :: Status) :: Constraint where
  Groupable 'Playing = ()
  Groupable 'Paused  = ()
  Groupable other    =
    TypeError
      ( 'Text "group membership requires playing/paused channel; got "
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

unload :: Audio s :> es => s Loaded -> Eff es (s Unloaded)
unload sound = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.unloadA sound

resume :: Audio s :> es => s Paused -> Eff es (s Playing)
resume channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.resumeA channel

play :: Audio s :> es => s Loaded -> Times -> Eff es (s Playing)
play sound t = do
  let normTimes = normalizeTimes t
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.playA sound normTimes

mkGroup :: Audio s :> es => String -> Eff es (Group s)
mkGroup name = do
  AudioRep AudioBackend{ mkGroupA = f } <- getStaticRep
  unsafeEff_ (f name)

addToGroup :: (Audio s :> es, Groupable st) => Group s -> s st -> Eff es ()
addToGroup group channel = do
  AudioRep AudioBackend{ addToGroupA = addGroup} <- getStaticRep
  unsafeEff_ (addGroup group channel)

removeFromGroup :: (Audio s :> es, Groupable st) => Group s -> s st -> Eff es ()
removeFromGroup group channel = do
  AudioRep AudioBackend{ removeFromGroupA = removeGroup } <- getStaticRep
  unsafeEff_ (removeGroup group channel)

pauseGroup :: Audio s :> es => Group s -> Eff es ()
pauseGroup group = do
  AudioRep AudioBackend{ pauseGroupA = pGroup } <- getStaticRep
  unsafeEff_ (pGroup group)

resumeGroup :: Audio s :> es => Group s -> Eff es ()
resumeGroup group = do
  AudioRep AudioBackend{ resumeGroupA = resGroup } <- getStaticRep
  unsafeEff_ (resGroup group)

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

getVolume :: (Audio s :> es, Adjustable st) => s st -> Eff es Volume
getVolume channel = do
  AudioRep AudioBackend{ getVolumeA = getVol } <- getStaticRep
  unsafeEff_ (getVol channel)

mute :: (Audio s :> es, Adjustable st) => s st -> Eff es ()
mute channel = setVolume channel (mkVolume 0.0)

setPanning :: (Audio s :> es, Adjustable st) => s st -> Panning -> Eff es ()
setPanning channel panning = do
  AudioRep AudioBackend{ setPanningA = setPan } <- getStaticRep
  unsafeEff_ (setPan channel panning)

getPanning :: (Audio s :> es, Adjustable st) => s st -> Eff es Panning
getPanning channel = do
  AudioRep AudioBackend{ getPanningA = getPan } <- getStaticRep
  unsafeEff_ (getPan channel)

hasFinished :: Audio s :> es => s Playing -> Eff es Bool
hasFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.hasFinishedA channel

awaitFinished :: Audio s :> es => s Playing -> Eff es ()
awaitFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.awaitFinishedA channel

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

defaultPanning :: Panning
defaultPanning = mkPanning 0.0

defaultVolume :: Volume
defaultVolume = mkVolume 1.0

normalizeTimes :: Times -> Times
normalizeTimes = \case
  Times n | n < 1 -> Once
  t               -> t
