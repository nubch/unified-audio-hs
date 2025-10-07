{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}

module Fmod.Backend
  ( runAudio
  , runAudioWith
  , FmodState
  , getAliveChannel
  ) where

----------------------------------------------------------------
-- Imports
----------------------------------------------------------------

-- Effectful
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Foreign ( FunPtr, freeHaskellFunPtr)
import Effectful (Eff, IOE, type (:>), withEffToIO)
import Effectful.Dispatch.Static
  ( evalStaticRep
  )

-- Interface / Safe layer
import qualified UnifiedAudio.Effectful as I
import qualified Fmod.Safe as Safe
import qualified Data.Map.Strict as Map
import Control.Concurrent
    ( MVar,
      newMVar,
      modifyMVar_,
      tryPutMVar,
      newEmptyMVar,
      isEmptyMVar,
      readMVar,
      modifyMVar,
      killThread,
      threadDelay,
      forkIO )

-- Concurrency / system
import Control.Exception ( mask, finally )
import Control.Monad (forever)
import System.IO (hFlush, stdout)

----------------------------------------------------------------
-- Backend wiring / Runner
----------------------------------------------------------------

-- | Construct the FMOD implementation for the unified audio interface.
makeBackendFmod :: EnvFMOD -> I.AudioBackend FmodState
makeBackendFmod env =
  I.AudioBackend
    { I.playA          = playFmod env,
      I.loadA          = loadFmod env,
      I.pauseA         = pauseFmod,
      I.resumeA        = resumeFmod,
      I.setVolumeA     = setVolumeFmod,
      I.getVolumeA     = getVolumeFmod,
      I.setPlacementA    = setPlacementFmod env,
      I.getPlacementA    = getPlacementFmod env,
      I.stopChannelA   = stopChannelFmod env,
      I.hasFinishedA   = hasFinishedFmod,
      I.unloadA        = unloadFmod,
      I.awaitFinishedA = awaitFinishedFmod,
      I.makeGroupA     = makeGroupFmod env,
      I.addToGroupA    = addToGroupFmod env,
      I.removeFromGroupA = removeFromGroupFmod env,
      I.pauseGroupA    = pauseGroupFmod env,
      I.resumeGroupA    = resumeGroupFmod env,
      I.stopGroupA      = stopGroupFmod env,
      I.isGroupPausedA  = isGroupPausedFmod env,
      I.setGroupVolumeA = setGroupVolumeFmod env,
      I.getGroupVolumeA = getGroupVolumeFmod env,
      I.setGroupPlacementA = setGroupPlacementFmod env,
      I.getGroupPlacementA = getGroupPlacementFmod env
    }

-- | Run the effect stack with the FMOD backend, driving system updates
-- at the specified frequency (Hz).
runAudioWith :: (IOE :> es) => Int -> Eff (I.Audio FmodState : es) a -> Eff es a
runAudioWith hz eff =
  withEffToIO $ \runInIO ->
    -- All FMOD lifetime in one scope:
    Safe.withSystem $ \sys -> mask $ \restore -> do
      -- allocate finished-callback, keep alive for whole session
      (finMap, pnMap, cb) <- Safe.setupFMODEnv

      gMap    <- newMVar Map.empty
      gpMap   <- newMVar Map.empty
      gPaused <- newMVar Map.empty
      gCounter <- newMVar 0
      let env     = EnvFMOD { system = sys
                            , finishMap = finMap
                            , panMap    = pnMap
                            , callback  = cb
                            , groupMap  = gMap
                            , groupPlacementMap = gpMap
                            , groupPausedMap = gPaused
                            , groupCounter = gCounter
                            }
          backend = makeBackendFmod env
          runApp  = runInIO (evalStaticRep (I.AudioRep backend) eff)

      pumpTid <- do
          let period = max 1 (1000000 `div` hz)
          forkIO $ forever $ Safe.systemUpdate sys >> threadDelay period 

      restore runApp `finally` do
        Safe.drainActive env.finishMap  -- setCallback NULL on any tracked channels
        killThread pumpTid
        hFlush stdout
        freeHaskellFunPtr cb

-- | Run the FMOD backend with a default 60 Hz update rate.
runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio = runAudioWith 60

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type GroupMap = MVar (Map.Map Int Safe.ChannelGroup)
type GroupPlacementMap = MVar (Map.Map Int I.Placement)

type Finished = MVar ()

data EnvFMOD = EnvFMOD
  { system      :: Safe.System
  , finishMap   :: Safe.FinishMap
  , panMap      :: Safe.PanMap
  , callback    :: FunPtr Safe.ChannelCB
  , groupMap    :: GroupMap
  , groupPlacementMap :: GroupPlacementMap
  , groupPausedMap  :: MVar (Map.Map Int Bool)
  , groupCounter :: MVar Int
  }

data FmodState :: I.Status -> Type where
  LoadedSound    :: Safe.Sound -> FmodState I.Loaded
  UnloadedSound  :: FmodState I.Unloaded
  PlayingChannel :: Safe.Channel -> Finished -> Safe.Sound -> FmodState I.Playing
  PausedChannel  :: Safe.Channel -> Finished -> Safe.Sound -> FmodState I.Paused
  StoppedChannel :: FmodState I.Stopped

----------------------------------------------------------------
-- Loading
----------------------------------------------------------------

-- | Load a sound from a file path or bytes using FMOD.
loadFmod :: EnvFMOD -> I.Source -> I.SoundType -> IO (FmodState I.Loaded)
loadFmod env src _ = case src of
  I.FromFile fp ->
    LoadedSound <$> Safe.createSound env.system fp
  I.FromBytes by ->
    LoadedSound <$> Safe.createSoundFromBytes env.system by

-- | Free FMOD resources for a previously loaded sound.
unloadFmod :: FmodState I.Loaded -> IO (FmodState I.Unloaded)
unloadFmod (LoadedSound sound) = do
   Safe.finalizeSound sound
   pure UnloadedSound

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

-- | Start playback for a loaded sound with the given loop mode.
-- Registers finish callback tracking and returns a playing handle.
playFmod :: EnvFMOD -> FmodState I.Loaded -> I.LoopMode -> IO (FmodState I.Playing)
playFmod env (LoadedSound sound) loopmode = do
  channel <- Safe.playSound env.system sound
  finished <- newEmptyMVar
  Safe.setChannelCallback channel env.callback
  Safe.withChannelPtr channel $ \pCh -> do
    modifyMVar_ env.finishMap (pure . Map.insert pCh finished)
    modifyMVar_ env.panMap  (pure . Map.insert pCh I.defaultPlacement)
  paused <- pauseFmod (PlayingChannel channel finished sound)
  applyLoopMode loopmode paused
  resumeFmod paused

-- | Configure loop mode for a paused channel according to 'I.LoopMode'.
applyLoopMode :: I.LoopMode -> FmodState I.Paused -> IO ()
applyLoopMode t (PausedChannel ch _ _) = case t of
  I.Once -> do
    Safe.setChannelMode ch Safe.LoopOff
    Safe.setLoopCount ch 0
  I.Forever -> do
    Safe.setChannelMode ch Safe.LoopNormal
    Safe.setLoopCount ch (-1)

-- | Set the FMOD channel's paused state and return the channel.
setPausedFmod :: Bool -> Safe.Channel -> IO Safe.Channel
setPausedFmod paused channel =
   Safe.setPaused paused channel >> return channel

-- | Pause a playing channel; returns a paused handle.
pauseFmod :: FmodState I.Playing -> IO (FmodState I.Paused)
pauseFmod (PlayingChannel channel finished sound) = do
  ch <- setPausedFmod True channel
  pure (PausedChannel ch finished sound)

-- | Resume a paused channel; returns a playing handle.
resumeFmod :: FmodState I.Paused -> IO (FmodState I.Playing)
resumeFmod (PausedChannel channel finished sound) = do
  ch <- setPausedFmod False channel
  pure (PlayingChannel ch finished sound)

-- | Stop a playing/paused channel and clean up tracking state.
stopChannelFmod :: forall alive. I.Alive alive => EnvFMOD -> FmodState alive -> IO (FmodState I.Stopped)
stopChannelFmod env stoppable = do
  case stoppable of
    (PlayingChannel channel finished _) -> stop channel finished
    (PausedChannel  channel finished _) -> stop channel finished
  where
    stop ch done = do
      Safe.withChannelPtr ch $ \pCh -> do
        modifyMVar_ env.finishMap (pure . Map.delete pCh)
        modifyMVar_ env.panMap (pure . Map.delete pCh)
      _ <- tryPutMVar done ()
      Safe.tryStopChannel ch
      pure StoppedChannel

-- | Check whether a playing channel has finished.
hasFinishedFmod :: FmodState I.Playing -> IO Bool
hasFinishedFmod (PlayingChannel _ finished _) = do
  fin <- isEmptyMVar finished
  pure $ not fin

-- | Block until the playing channel has finished.
awaitFinishedFmod :: FmodState I.Playing -> IO ()
awaitFinishedFmod (PlayingChannel _ finished _) =
  readMVar finished

----------------------------------------------------------------
-- Volume / Placement
----------------------------------------------------------------

-- | Set the channel's volume (0..1).
setVolumeFmod :: forall alive. I.Alive alive => FmodState alive -> I.Volume -> IO ()
setVolumeFmod adjustable volume = do
  let ch = getAliveChannel adjustable
  Safe.setVolume ch (realToFrac $ I.unVolume volume)

-- | Set the channel's logical Placement (-1..1) and update tracking map.
setPlacementFmod :: forall alive. I.Alive alive => EnvFMOD -> FmodState alive -> I.Placement -> IO ()
setPlacementFmod env adjustable placement = do
  let ch = getAliveChannel adjustable
  Safe.setPlacement ch (realToFrac $ I.unPlacement placement)
  Safe.withChannelPtr ch $ \pCh ->
    modifyMVar_ env.panMap $ pure . Map.adjust (const placement) pCh

-- | Get the channel's current volume.
getVolumeFmod :: forall alive. I.Alive alive => FmodState alive -> IO I.Volume
getVolumeFmod s = do
  let ch = getAliveChannel s
  I.mkVolume <$> Safe.getChannelVolume ch

-- | Get the channel's logical Placement (tracked; defaults to center when missing).
getPlacementFmod :: forall alive. EnvFMOD -> I.Alive alive => FmodState alive -> IO I.Placement
getPlacementFmod env s = do
  let ch = getAliveChannel s
  Safe.withChannelPtr ch $ \pCh -> do
    m <- readMVar env.panMap
    pure $ fromMaybe I.defaultPlacement (Map.lookup pCh m)

----------------------------------------------------------------
-- Groups (FMOD)
----------------------------------------------------------------

-- | Create a new FMOD channel group and initialize tracking state.
makeGroupFmod :: EnvFMOD -> IO (I.Group FmodState)
makeGroupFmod env = do
  gid <- modifyMVar env.groupCounter $ \n ->
    let next = n + 1
    in pure (next, n)
  let groupName = "group-" ++ show gid
  grp <- Safe.createChannelGroup env.system groupName
  modifyMVar_ env.groupMap $ \gm -> pure (Map.insert gid grp gm)
  -- initialize group Placement tracking to default (0)
  modifyMVar_ env.groupPlacementMap $ \gpm -> pure (Map.insert gid I.defaultPlacement gpm)
  -- initialize query state
  modifyMVar_ env.groupPausedMap $ \pm -> pure (Map.insert gid False pm)
  pure (I.GroupId gid)

-- | Add a channel to a group.
addToGroupFmod :: forall alive. I.Alive alive => EnvFMOD -> I.Group FmodState -> FmodState alive -> IO ()
addToGroupFmod env (I.GroupId gid) s = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just grp -> do
      let ch = getAliveChannel s
      Safe.setChannelGroup ch grp

-- | Remove a channel from its group by moving it back to the master group.
removeFromGroupFmod :: forall alive. I.Alive alive => EnvFMOD -> I.Group FmodState -> FmodState alive -> IO ()
removeFromGroupFmod env _ s = do
  master <- Safe.getMasterChannelGroup env.system
  let ch = getAliveChannel s
  Safe.setChannelGroup ch master

-- | Mark a group as paused (and record tracking state).
pauseGroupFmod :: EnvFMOD -> I.Group FmodState -> IO ()
pauseGroupFmod env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure ()
    Just grp -> do
      Safe.setGroupPaused grp True
      modifyMVar_ env.groupPausedMap $ \pm -> pure (Map.insert gid True pm)

-- | Resume a paused group and clear channel-level pause for its members.
resumeGroupFmod :: EnvFMOD -> I.Group FmodState -> IO ()
resumeGroupFmod env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure ()
    Just grp -> do
      Safe.setGroupPaused grp False
      modifyMVar_ env.groupPausedMap $ \pm -> pure (Map.insert gid False pm)
      -- Force-clear channel-level pause for all channels in this group
      chans <- Safe.getGroupChannels grp
      mapM_ (\ch -> Safe.setPaused False ch) chans

-- | Set a group's volume.
setGroupVolumeFmod :: EnvFMOD -> I.Group FmodState -> I.Volume -> IO ()
setGroupVolumeFmod env (I.GroupId gid) vol = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure ()
    Just grp -> Safe.setGroupVolume grp (realToFrac $ I.unVolume vol)

-- | Set a group's Placement and update tracking state.
setGroupPlacementFmod :: EnvFMOD -> I.Group FmodState -> I.Placement -> IO ()
setGroupPlacementFmod env (I.GroupId gid) pan = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure ()
    Just grp -> do
      Safe.setGroupPlacement grp (realToFrac $ I.unPlacement pan)
      modifyMVar_ env.groupPlacementMap $ \gpm -> pure (Map.insert gid pan gpm)

-- | Stop all channels in a group.
stopGroupFmod :: EnvFMOD -> I.Group FmodState -> IO ()
stopGroupFmod env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure ()
    Just grp -> do
      Safe.stopGroup grp

-- | Get a group's volume.
getGroupVolumeFmod :: EnvFMOD -> I.Group FmodState -> IO I.Volume
getGroupVolumeFmod env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing  -> pure I.defaultVolume
    Just grp -> I.mkVolume <$> Safe.getGroupVolume grp

-- | Get a group's logical Placement (tracked; default center).
getGroupPlacementFmod :: EnvFMOD -> I.Group FmodState -> IO I.Placement
getGroupPlacementFmod env (I.GroupId gid) = do
  gpm <- readMVar env.groupPlacementMap
  pure (Map.findWithDefault I.defaultPlacement gid gpm)

-- Queries
-- | Check if a group is marked paused (tracked state).
isGroupPausedFmod :: EnvFMOD -> I.Group FmodState -> IO Bool
isGroupPausedFmod env (I.GroupId gid) = do
  pm <- readMVar env.groupPausedMap
  pure (Map.findWithDefault False gid pm)

-- removed: isGroupStoppedFmod (no longer part of API)

----------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------

-- | Extract the underlying 'Safe.Channel' from a playing/paused handle.
getAliveChannel :: I.Alive alive => FmodState alive -> Safe.Channel
getAliveChannel s = case s of
  PlayingChannel ch _ _ -> ch
  PausedChannel  ch _ _ -> ch
