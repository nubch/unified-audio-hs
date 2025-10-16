{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: UnifiedAudio.Effectful
-- Description: Effectful-based audio API with typed channel states and grouping.
--
-- This module defines an effect interface for audio backends using the
-- @effectful@ library. Channel handles carry type-level status (e.g. 'Loaded',
-- 'Playing') to make invalid state transitions unrepresentable. Operations are
-- provided for loading, playback control, per-channel parameters, and group-wide
-- controls.
module UnifiedAudio.Effectful
  ( -- * Effect and backend wiring
    Audio,
    AudioBackend (..),
    type Alive,
    pattern AudioRep,
    -- * Core types
    Source (..),
    Status (..),
    SoundType (..),
    LoopMode (..),
    Group (..),
    Volume,
    Placement,
    -- * Loading / lifecycle
    loadFile,
    loadBytes,
    unload,
    -- * Playback
    play,
    pause,
    resume,
    stop,
    hasFinished,
    awaitFinished,
    -- * Channel Adjustments
    setVolume,
    getVolume,
    mute,
    setPlacement,
    getPlacement,
    -- * Groups
    makeGroup,
    addToGroup,
    removeFromGroup,
    pauseGroup,
    resumeGroup,
    stopGroup,
    isGroupPaused,
    -- * Group Adjustments
    setGroupVolume,
    getGroupVolume,
    setGroupPlacement,
    getGroupPlacement,
    -- * Utilities
    mkVolume,
    unVolume,
    mkPlacement,
    unPlacement,
    defaultVolume,
    defaultPlacement,
  )
where

----------------------------------------------------------------
-- Imports
----------------------------------------------------------------

import qualified Data.ByteString as BS
import Data.Kind (Type)
-- Effectful core
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )
-- Effectful dispatcher (static)
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    getStaticRep,
    unsafeEff_,
  )

import GHC.Exts (Constraint)
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | A source of audio data used to create 'Loaded' sounds.
data Source
  = -- | Load from a filesystem path.
    FromFile FilePath
  | -- | Load from bytes.
    FromBytes BS.ByteString

-- | A group of channels for batch control operations.
newtype Group (s :: Status -> Type)
  = -- | Opaque group identifier used internally.
    GroupId Int
  deriving (Show, Eq)

-- | Type-level status of a channel or sound resource.
data Status = Loaded | Playing | Paused | Stopped | Unloaded deriving (Show, Eq)

-- | Intended channel layout to use when loading a sound.
-- Used for identifying between panning or balancing the channel.
data SoundType = Mono | Stereo
  deriving (Show, Eq)

-- | Looping behavior for playback.
data LoopMode = Once | Forever
  deriving (Show, Eq)

----------------------------------------------------------------
-- Effect / Backend wiring
----------------------------------------------------------------

-- | Audio effect keyed by a backend @s@ that carries typed channel states.
data Audio (s :: Status -> Type) :: Effect

-- | Backend implementation for the 'Audio' effect.
data AudioBackend (s :: Status -> Type) = AudioBackend
  { -- | Load a sound from a 'Source'.
    loadA :: Source -> SoundType -> IO (s 'Loaded),
    -- | Unload a loaded sound.
    unloadA :: s 'Loaded -> IO (s 'Unloaded),
    -- | Start playback with the given loop mode.
    playA :: s 'Loaded -> LoopMode -> IO (s 'Playing),
    -- | Pause a playing channel.
    pauseA :: s 'Playing -> IO (s 'Paused),
    -- | Resume a paused channel.
    resumeA :: s 'Paused -> IO (s 'Playing),
    -- | Set channel volume.
    setVolumeA :: forall alive. (Alive alive) => s alive -> Volume -> IO (),
    -- | Get channel volume.
    getVolumeA :: forall alive. (Alive alive) => s alive -> IO Volume,
    -- | Set channel stereo placement.
    setPlacementA :: forall alive. (Alive alive) => s alive -> Placement -> IO (),
    -- | Get channel stereo placement.
    getPlacementA :: forall alive. (Alive alive) => s alive -> IO Placement,
    -- | Stop a playing or paused channel.
    stopChannelA :: forall alive. (Alive alive) => s alive -> IO (s 'Stopped),
    -- | Query if a playing channel has finished naturally.
    hasFinishedA :: s 'Playing -> IO Bool,
    -- | Block until a playing channel finishes.
    awaitFinishedA :: s 'Playing -> IO (),
    -- | Create an empty group.
    makeGroupA :: IO (Group s),
    -- | Add a channel to a group.
    addToGroupA :: forall alive. (Alive alive) => Group s -> s alive -> IO (),
    -- | Remove a channel from a group.
    removeFromGroupA :: forall alive. (Alive alive) => Group s -> s alive -> IO (),
    -- | Pause all channels in a group.
    pauseGroupA :: Group s -> IO (),
    -- | Resume all channels in a group.
    resumeGroupA :: Group s -> IO (),
    -- | Stop all channels in a group.
    stopGroupA :: Group s -> IO (),
    -- | Check whether a group is paused.
    isGroupPausedA :: Group s -> IO Bool,
    -- | Set group volume.
    setGroupVolumeA :: Group s -> Volume -> IO (),
    -- | Get group volume.
    getGroupVolumeA :: Group s -> IO Volume,
    -- | Set group stereo placement.
    setGroupPlacementA :: Group s -> Placement -> IO (),
    -- | Get group stereo placement.
    getGroupPlacementA :: Group s -> IO Placement
  }

-- | 'Audio' uses a static dispatcher with side effects.
type instance DispatchOf (Audio s) = Static WithSideEffects

-- | Static representation stores the backend functions.
newtype instance StaticRep (Audio s) = AudioRep (AudioBackend s)

-- | Normalized linear amplitude in [0,1]. Use 'mkVolume' and 'unVolume'.
newtype Volume = Volume Float deriving (Show, Eq) -- Only values between 0 and 1

-- | Stereo pan in [-1,1]; -1 is left, 0 center, 1 right. Use 'mkPlacement' and 'unPlacement'.
newtype Placement = Placement Float deriving (Show, Eq)

-- | Single constraint family for operations on active channels (playing/paused).
type family Alive (st :: Status) :: Constraint where
  Alive 'Playing = ()
  Alive 'Paused = ()
  Alive other =
    TypeError
      ( 'Text "operation requires a playing or paused channel; got "
          ':<>: 'ShowType other
      )


----------------------------------------------------------------
-- Loading / Unloading Sounds
----------------------------------------------------------------

-- | Loads a sound from a 'FilePath'.
-- Returns a handle in the 'Loaded' state managed by the backend.
loadFile :: (Audio s :> es) => FilePath -> SoundType -> Eff es (s Loaded)
loadFile fp stype = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.loadA (FromFile fp) stype

-- | Loads a sound from a 'ByteString'.
-- Returns a handle in the 'Loaded' state managed by the backend.
loadBytes :: (Audio s :> es) => BS.ByteString -> SoundType -> Eff es (s Loaded)
loadBytes bs stype = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.loadA (FromBytes bs) stype

-- | Unload a previously 'Loaded' sound, releasing its resources.
-- Transitions the handle from 'Loaded' to 'Unloaded'.
unload :: (Audio s :> es) => s Loaded -> Eff es (s Unloaded)
unload sound = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.unloadA sound

----------------------------------------------------------------
-- Playing Sounds
----------------------------------------------------------------

-- | Start playback of a 'Loaded' sound with the given 'LoopMode'.
-- Transitions the handle from 'Loaded' to 'Playing'.
play :: (Audio s :> es) => s Loaded -> LoopMode -> Eff es (s Playing)
play sound t = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.playA sound t

----------------------------------------------------------------
-- Channel Operations
----------------------------------------------------------------

-- | Pause a 'Playing' channel, transitioning it to 'Paused'.
pause :: (Audio s :> es) => s Playing -> Eff es (s Paused)
pause channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.pauseA channel

-- | Resume a 'Paused' channel, transitioning it back to 'Playing'.
resume :: (Audio s :> es) => s Paused -> Eff es (s Playing)
resume channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.resumeA channel

-- | Stop an "alive" channel ('Playing' or 'Paused'), transitioning it to 'Stopped'.
stop :: (Audio s :> es, Alive alive) => s alive -> Eff es (s 'Stopped)
stop channel = do
  AudioRep AudioBackend {stopChannelA = stop'} <- getStaticRep
  unsafeEff_ (stop' channel)

-- | Set the channel 'Volume'.
setVolume :: (Audio s :> es, Alive alive) => s alive -> Volume -> Eff es ()
setVolume channel volume = do
  AudioRep AudioBackend {setVolumeA = setVol} <- getStaticRep
  unsafeEff_ (setVol channel volume)

-- | Get the current channel 'Volume'.
getVolume :: (Audio s :> es, Alive alive) => s alive -> Eff es Volume
getVolume channel = do
  AudioRep AudioBackend {getVolumeA = getVol} <- getStaticRep
  unsafeEff_ (getVol channel)

-- | Convenience to mute a channel (sets volume to 0.0).
mute :: (Audio s :> es, Alive alive) => s alive -> Eff es ()
mute channel = setVolume channel (mkVolume 0.0)

-- | Set the channel stereo 'Placement'.
setPlacement :: (Audio s :> es, Alive alive) => s alive -> Placement -> Eff es ()
setPlacement channel placement = do
  AudioRep AudioBackend {setPlacementA = setPan} <- getStaticRep
  unsafeEff_ (setPan channel placement)

-- | Get the current per-channel stereo 'Placement'.
getPlacement :: (Audio s :> es, Alive alive) => s alive -> Eff es Placement
getPlacement channel = do
  AudioRep AudioBackend {getPlacementA = getPan} <- getStaticRep
  unsafeEff_ (getPan channel)

-- | Check whether a 'Playing' channel has naturally finished playback.
hasFinished :: (Audio s :> es) => s Playing -> Eff es Bool
hasFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.hasFinishedA channel

-- | Block until the 'Playing' channel finishes playback.
awaitFinished :: (Audio s :> es) => s Playing -> Eff es ()
awaitFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.awaitFinishedA channel

----------------------------------------------------------------
-- Group Operations
----------------------------------------------------------------

-- | Create an empty 'Group' for batch operations over channels.
makeGroup :: (Audio s :> es) => Eff es (Group s)
makeGroup = do
  AudioRep AudioBackend {makeGroupA = f} <- getStaticRep
  unsafeEff_ f

-- | Add a channel in an "alive" state ('Playing' or 'Paused') to a 'Group'.
-- This will remove the channel from the group it was previously assigned to.
addToGroup :: (Audio s :> es, Alive alive) => Group s -> s alive -> Eff es ()
addToGroup group channel = do
  AudioRep AudioBackend {addToGroupA = addGroup} <- getStaticRep
  unsafeEff_ (addGroup group channel)

-- | Remove a channel from a 'Group'.
removeFromGroup :: (Audio s :> es, Alive alive) => Group s -> s alive -> Eff es ()
removeFromGroup group channel = do
  AudioRep AudioBackend {removeFromGroupA = removeGroup} <- getStaticRep
  unsafeEff_ (removeGroup group channel)

-- | Pause a 'Group' and subsequently all of its members.
pauseGroup :: (Audio s :> es) => Group s -> Eff es ()
pauseGroup group = do
  AudioRep AudioBackend {pauseGroupA = pGroup} <- getStaticRep
  unsafeEff_ (pGroup group)

-- | Resume a 'Group' and subsequently all of its members.
resumeGroup :: (Audio s :> es) => Group s -> Eff es ()
resumeGroup group = do
  AudioRep AudioBackend {resumeGroupA = resGroup} <- getStaticRep
  unsafeEff_ (resGroup group)

-- | Stop all channels in a 'Group', and subsequently remove them from the group.
stopGroup :: (Audio s :> es) => Group s -> Eff es ()
stopGroup group = do
  AudioRep AudioBackend {stopGroupA = sGroup} <- getStaticRep
  unsafeEff_ (sGroup group)

-- | Set the 'Volume' for a 'Group'.
setGroupVolume :: (Audio s :> es) => Group s -> Volume -> Eff es ()
setGroupVolume group vol = do
  AudioRep AudioBackend {setGroupVolumeA = setGV} <- getStaticRep
  unsafeEff_ (setGV group vol)

-- | Get the current 'Volume' for a 'Group'.
getGroupVolume :: (Audio s :> es) => Group s -> Eff es Volume
getGroupVolume group = do
  AudioRep AudioBackend {getGroupVolumeA = getGV} <- getStaticRep
  unsafeEff_ (getGV group)

-- | Set the stereo 'Placement' of a 'Group'.
setGroupPlacement :: (Audio s :> es) => Group s -> Placement -> Eff es ()
setGroupPlacement group pan = do
  AudioRep AudioBackend {setGroupPlacementA = setGP} <- getStaticRep
  unsafeEff_ (setGP group pan)

-- | Get the current stereo 'Placement' of a 'Group'.
getGroupPlacement :: (Audio s :> es) => Group s -> Eff es Placement
getGroupPlacement group = do
  AudioRep AudioBackend {getGroupPlacementA = getGP} <- getStaticRep
  unsafeEff_ (getGP group)

-- | Check whether a 'Group' is currently paused.
isGroupPaused :: (Audio s :> es) => Group s -> Eff es Bool
isGroupPaused group = do
  AudioRep AudioBackend {isGroupPausedA = getPaused} <- getStaticRep
  unsafeEff_ (getPaused group)


--- Utilities

-- | Construct a clamped 'Placement' from a raw value, input is clamped to [-1,1].
mkPlacement :: Float -> Placement
mkPlacement x = Placement (clamp x)
  where
    clamp = max (-1.0) . min 1.0

-- | Extract the underlying 'Float' from a 'Placement'.
unPlacement :: Placement -> Float
unPlacement (Placement x) = x

-- | Construct a clamped 'Volume' from a raw value, input is clamped to [0,1].
mkVolume :: Float -> Volume
mkVolume x = Volume (clamp x)
  where
    clamp = max 0.0 . min 1.0

-- | Extract the underlying 'Float' from a 'Volume'.
unVolume :: Volume -> Float
unVolume (Volume v) = v

-- | Default, centered 'Placement' (0.0).
defaultPlacement :: Placement
defaultPlacement = mkPlacement 0.0

-- | Default, full 'Volume' (1.0).
defaultVolume :: Volume
defaultVolume = mkVolume 1.0