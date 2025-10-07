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

newtype Group (s :: Status -> Type) = GroupId Int
  deriving (Show, Eq)

data Status = Loaded | Playing | Paused | Stopped | Unloaded
  deriving (Show, Eq)

data SoundType = Mono | Stereo
  deriving (Show, Eq)

data LoopMode = Once | Forever
  deriving (Show, Eq)

--- Effectful

data Audio (s :: Status -> Type) :: Effect

data AudioBackend (s :: Status -> Type) = AudioBackend
  { 
    loadA            :: Source -> SoundType -> IO (s 'Loaded)
  , playA            :: s 'Loaded -> LoopMode -> IO (s 'Playing)
  , pauseA           :: s 'Playing -> IO (s 'Paused)
  , resumeA          :: s 'Paused  -> IO (s 'Playing)

  , setVolumeA       :: forall alive. Alive alive => s alive -> Volume  -> IO ()
  , getVolumeA       :: forall alive. Alive alive => s alive -> IO Volume
  , setPlacementA      :: forall alive. Alive alive => s alive -> Placement -> IO ()
  , getPlacementA      :: forall alive. Alive alive => s alive -> IO Placement
  , stopChannelA     :: forall alive. Alive alive => s alive -> IO (s 'Stopped)

  , hasFinishedA     :: s 'Playing -> IO Bool
  , awaitFinishedA   :: s 'Playing -> IO ()
  , unloadA          :: s 'Loaded  -> IO (s 'Unloaded)
  , makeGroupA       :: IO (Group s)
  , addToGroupA      :: forall alive. Alive alive => Group s -> s alive -> IO ()
  , removeFromGroupA :: forall alive. Alive alive => Group s -> s alive -> IO ()
  , pauseGroupA      :: Group s -> IO ()
  , resumeGroupA     :: Group s -> IO ()
  , stopGroupA       :: Group s -> IO ()
  , isGroupPausedA   :: Group s -> IO Bool
  , setGroupVolumeA  :: Group s -> Volume -> IO ()
  , getGroupVolumeA  :: Group s -> IO Volume
  , setGroupPlacementA :: Group s -> Placement -> IO ()
  , getGroupPlacementA :: Group s -> IO Placement
  }

type instance DispatchOf (Audio s) = Static WithSideEffects
newtype instance StaticRep (Audio s) = AudioRep (AudioBackend s)

newtype Volume = Volume Float deriving (Show, Eq) -- Only values between 0 and 1

newtype Placement = Placement Float deriving (Show, Eq)

-- Single constraint family for operations on active channels (playing/paused)
type family Alive (st :: Status) :: Constraint where
  Alive 'Playing = ()
  Alive 'Paused  = ()
  Alive other    =
    TypeError
      ( 'Text "operation requires a playing or paused channel; got "
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

play :: Audio s :> es => s Loaded -> LoopMode -> Eff es (s Playing)
play sound t = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.playA sound t

playOnce :: Audio s :> es => s Loaded -> Eff es (s Playing)
playOnce sound = play sound Once

playForever :: Audio s :> es => s Loaded -> Eff es (s Playing)
playForever sound = play sound Forever

makeGroup :: Audio s :> es => Eff es (Group s)
makeGroup = do
  AudioRep AudioBackend{ makeGroupA = f } <- getStaticRep
  unsafeEff_ f

addToGroup :: (Audio s :> es, Alive alive) => Group s -> s alive -> Eff es ()
addToGroup group channel = do
  AudioRep AudioBackend{ addToGroupA = addGroup} <- getStaticRep
  unsafeEff_ (addGroup group channel)

removeFromGroup :: (Audio s :> es, Alive alive) => Group s -> s alive -> Eff es ()
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

stopGroup :: Audio s :> es => Group s -> Eff es ()
stopGroup group = do
  AudioRep AudioBackend{ stopGroupA = sGroup } <- getStaticRep
  unsafeEff_ (sGroup group)

setGroupVolume :: Audio s :> es => Group s -> Volume -> Eff es ()
setGroupVolume group vol = do
  AudioRep AudioBackend{ setGroupVolumeA = setGV } <- getStaticRep
  unsafeEff_ (setGV group vol)

getGroupVolume :: Audio s :> es => Group s -> Eff es Volume
getGroupVolume group = do
  AudioRep AudioBackend{ getGroupVolumeA = getGV } <- getStaticRep
  unsafeEff_ (getGV group)

setGroupPlacement :: Audio s :> es => Group s -> Placement -> Eff es ()
setGroupPlacement group pan = do
  AudioRep AudioBackend{ setGroupPlacementA = setGP } <- getStaticRep
  unsafeEff_ (setGP group pan)

getGroupPlacement :: Audio s :> es => Group s -> Eff es Placement
getGroupPlacement group = do
  AudioRep AudioBackend{ getGroupPlacementA = getGP } <- getStaticRep
  unsafeEff_ (getGP group)

isGroupPaused :: Audio s :> es => Group s -> Eff es Bool
isGroupPaused group = do
  AudioRep AudioBackend{ isGroupPausedA = getPaused } <- getStaticRep
  unsafeEff_ (getPaused group)

pause :: Audio s :> es => s Playing -> Eff es (s Paused)
pause channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.pauseA channel

stop :: (Audio s :> es, Alive alive) => s alive -> Eff es (s 'Stopped)
stop channel = do
  AudioRep AudioBackend{ stopChannelA = stop' } <- getStaticRep
  unsafeEff_ (stop' channel)

setVolume :: (Audio s :> es, Alive alive) => s alive -> Volume -> Eff es ()
setVolume channel volume = do
  AudioRep AudioBackend{ setVolumeA = setVol } <- getStaticRep
  unsafeEff_ (setVol channel volume)

getVolume :: (Audio s :> es, Alive alive) => s alive -> Eff es Volume
getVolume channel = do
  AudioRep AudioBackend{ getVolumeA = getVol } <- getStaticRep
  unsafeEff_ (getVol channel)

mute :: (Audio s :> es, Alive alive) => s alive -> Eff es ()
mute channel = setVolume channel (mkVolume 0.0)

setPlacement :: (Audio s :> es, Alive alive) => s alive -> Placement -> Eff es ()
setPlacement channel placement = do
  AudioRep AudioBackend{ setPlacementA = setPan } <- getStaticRep
  unsafeEff_ (setPan channel placement)

getPlacement :: (Audio s :> es, Alive alive) => s alive -> Eff es Placement
getPlacement channel = do
  AudioRep AudioBackend{ getPlacementA = getPan } <- getStaticRep
  unsafeEff_ (getPan channel)

hasFinished :: Audio s :> es => s Playing -> Eff es Bool
hasFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.hasFinishedA channel

awaitFinished :: Audio s :> es => s Playing -> Eff es ()
awaitFinished channel = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ backend.awaitFinishedA channel

playOnGroup :: Audio s :> es => s Loaded -> Group s -> LoopMode ->  Eff es (s Playing)
playOnGroup sound group loopMode = do
  AudioRep AudioBackend{ addToGroupA = addGroup } <- getStaticRep
  channel <- play sound loopMode
  unsafeEff_ (addGroup group channel)
  pure channel

--- Utilities

mkPlacement :: Float -> Placement
mkPlacement x = Placement (clamp x)
  where clamp = max (-1.0) . min 1.0

unPlacement :: Placement -> Float
unPlacement (Placement x) = x

mkVolume :: Float -> Volume
mkVolume x = Volume (clamp x)
  where clamp = max 0.0 . min 1.0

unVolume :: Volume -> Float
unVolume (Volume v) = v

defaultPlacement :: Placement
defaultPlacement = mkPlacement 0.0

defaultVolume :: Volume
defaultVolume = mkVolume 1.0