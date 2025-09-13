{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}

module Fmod.Backend (runAudio) where

----------------------------------------------------------------
-- Imports
----------------------------------------------------------------

-- Effectful
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Foreign ( FunPtr, freeHaskellFunPtr)
import Effectful (Eff, IOE, type (:>), withEffToIO)
import Effectful.Dispatch.Static
  ( evalStaticRep, unsafeEff_
  )

-- Interface / Safe layer
import qualified UnifiedAudio.Effectful as I
import qualified Fmod.Safe as Safe
import qualified Data.Map.Strict as Map
import Control.Concurrent

-- Concurrency / system
import Control.Exception ( mask, finally )
import Control.Concurrent.MVar
    ( MVar,
      modifyMVar_,
      isEmptyMVar,
      newEmptyMVar,
      readMVar,
      takeMVar,
      putMVar,
      tryPutMVar,
      newMVar )
import Fmod.Safe (setLoopCount)
import System.IO (hFlush, stdout)

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type GroupMap = MVar (Map.Map String Safe.ChannelGroup)

data EnvFMOD = EnvFMOD
  { system      :: Safe.System
  , finishMap   :: Safe.FinishMap
  , panMap      :: Safe.PanMap
  , callback    :: FunPtr Safe.ChannelCB
  , groupMap    :: GroupMap
  }

data FmodState :: I.Status -> Type where
  LoadedSound  :: Safe.Sound -> FmodState I.Loaded
  UnloadedSound :: FmodState I.Unloaded
  PlayingSound :: Safe.Channel -> MVar () -> Safe.Sound -> FmodState I.Playing
  PausedSound  :: Safe.Channel -> MVar () -> Safe.Sound -> FmodState I.Paused
  StoppedSound :: Safe.Channel -> FmodState I.Stopped

----------------------------------------------------------------
-- Loading
----------------------------------------------------------------

loadFmod :: EnvFMOD -> I.Source -> I.SoundType -> IO (FmodState I.Loaded)
loadFmod env src _ = case src of
  I.FromFile fp ->
    LoadedSound <$> Safe.createSound env.system fp
  I.FromBytes by ->
    LoadedSound <$> Safe.createSoundFromBytes env.system by

unloadFmod :: FmodState I.Loaded -> IO (FmodState I.Unloaded)
unloadFmod (LoadedSound sound) = do
   Safe.finalizeSound sound
   pure UnloadedSound

--updateFmod :: EnvFMOD -> FmodState I.Loaded -> IO ()
--updateFmod env (LoadedSound sound) = Safe.systemUpdate env.system sound

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

playFmod :: EnvFMOD -> FmodState I.Loaded -> I.Times -> IO (FmodState I.Playing)
playFmod env (LoadedSound sound) times = do
  channel <- Safe.playSound env.system sound
  finished <- newEmptyMVar
  Safe.setChannelCallback channel env.callback
  Safe.withChannelPtr channel $ \pCh -> do
    modifyMVar_ env.finishMap (pure . Map.insert pCh finished)
    modifyMVar_ env.panMap  (pure . Map.insert pCh I.defaultPanning)
  paused <- pauseFmod (PlayingSound channel finished sound)
  applyTimes times paused
  resumeFmod paused

applyTimes :: I.Times -> FmodState I.Paused -> IO ()
applyTimes t (PausedSound ch _ _) = case t of
  I.Once -> do
    Safe.setChannelMode ch Safe.LoopOff
    setLoopCount ch 0
  I.Times n -> do
    Safe.setChannelMode ch Safe.LoopNormal
    setLoopCount ch (n - 1)
  I.Forever -> do
    Safe.setChannelMode ch Safe.LoopNormal
    setLoopCount ch (-1)

setPausedFmod :: Bool -> Safe.Channel -> IO Safe.Channel
setPausedFmod paused channel =
   Safe.setPaused paused channel >> return channel

pauseFmod :: FmodState I.Playing -> IO (FmodState I.Paused)
pauseFmod (PlayingSound channel finished sound) = do
  ch <- setPausedFmod True channel
  pure (PausedSound ch finished sound)

resumeFmod :: FmodState I.Paused -> IO (FmodState I.Playing)
resumeFmod (PausedSound channel finished sound) = do
  ch <- setPausedFmod False channel
  pure (PlayingSound ch finished sound)

stopChannelFmod :: forall st. I.Stoppable st => EnvFMOD -> FmodState st -> IO (FmodState I.Stopped)
stopChannelFmod env stoppable = do
  case stoppable of
    (PlayingSound channel finished _) -> stop channel finished
    (PausedSound  channel finished _) -> stop channel finished
  where
    stop ch done = do
      Safe.withChannelPtr ch $ \pCh -> do
        modifyMVar_ env.finishMap (pure . Map.delete pCh)
        modifyMVar_ env.panMap (pure . Map.delete pCh)
      _ <- tryPutMVar done ()
      Safe.tryStopChannel ch
      pure (StoppedSound ch)

hasFinishedFmod :: FmodState I.Playing -> IO Bool
hasFinishedFmod (PlayingSound _ finished _) = do
  fin <- isEmptyMVar finished
  pure $ not fin

awaitFinishedFmod :: FmodState I.Playing -> IO ()
awaitFinishedFmod (PlayingSound _ finished _) =
  takeMVar finished

----------------------------------------------------------------
-- Volume / Panning
----------------------------------------------------------------

setVolumeFmod :: forall adj. I.Adjustable adj => FmodState adj -> I.Volume -> IO ()
setVolumeFmod adjustable volume =
  case adjustable of
    (PlayingSound playing _ _) -> setVolume playing volume
    (PausedSound  playing _ _) -> setVolume playing volume
  where
    setVolume ch vol = Safe.setVolume ch (realToFrac $ I.unVolume vol)

setPanningFmod :: forall adj. I.Adjustable adj => EnvFMOD -> FmodState adj -> I.Panning -> IO ()
setPanningFmod env adjustable panning =
  case adjustable of
    (PlayingSound playing _ _) -> setPanning playing panning
    (PausedSound  playing _ _) -> setPanning playing panning
  where
    setPanning ch pan = do
      Safe.setPanning ch (realToFrac $ I.unPanning pan)
      Safe.withChannelPtr ch $ \pCh ->
        modifyMVar_ env.panMap $ pure . Map.adjust (const pan) pCh

getVolumeFmod :: forall st. I.Adjustable st => FmodState st -> IO I.Volume
getVolumeFmod s = case s of
  (PlayingSound ch _ _) -> I.mkVolume <$> Safe.getChannelVolume ch
  (PausedSound  ch _ _) -> I.mkVolume <$> Safe.getChannelVolume ch

getPanningFmod :: forall st. EnvFMOD -> I.Adjustable st => FmodState st -> IO I.Panning
getPanningFmod env s = case s of
  (PlayingSound ch _ _) -> getPan ch
  (PausedSound  ch _ _) -> getPan ch
  where
    getPan ch = Safe.withChannelPtr ch $ \pCh -> do
      m <- readMVar env.panMap
      pure $ fromMaybe I.defaultPanning (Map.lookup pCh m)

----------------------------------------------------------------
-- Backend wiring / Runner
----------------------------------------------------------------

makeBackendFmod :: EnvFMOD -> I.AudioBackend FmodState
makeBackendFmod env =
  I.AudioBackend
    { I.playA          = playFmod env,
      I.loadA          = loadFmod env,
      I.pauseA         = pauseFmod,
      I.resumeA        = resumeFmod,
      I.setVolumeA     = setVolumeFmod,
      I.getVolumeA     = getVolumeFmod,
      I.setPanningA    = setPanningFmod env,
      I.getPanningA    = getPanningFmod env,
      I.stopChannelA   = stopChannelFmod env,
      I.hasFinishedA   = hasFinishedFmod,
      I.unloadA        = unloadFmod,
      I.awaitFinishedA = awaitFinishedFmod,
      I.mkOrGetGroupA  = mkOrGetGroupFmod env,
      I.addToGroupA    = addToGroupFmod env,
      I.removeFromGroupA = removeFromGroupFmod env,
      I.pauseGroupA    = pauseGroupFmod env,
      I.resumeGroupA    = resumeGroupFmod env,
      I.setGroupVolumeA = setGroupVolumeFmod env,
      I.setGroupPanningA = setGroupPanningFmod env
    }

runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO ->
    -- All FMOD lifetime in one scope:
    Safe.withSystem $ \sys -> mask $ \restore -> do
      -- allocate finished-callback, keep alive for whole session
      (finMap, pnMap, cb) <- Safe.setupFMODEnv

      gMap    <- newMVar Map.empty
      let env     = EnvFMOD { system = sys
                            , finishMap = finMap
                            , panMap    = pnMap
                            , callback  = cb
                            , groupMap  = gMap
                            }
          backend = makeBackendFmod env
          runApp  = runInIO (evalStaticRep (I.AudioRep backend) eff)
      pumpTid <- forkIO $ let loop = Safe.systemUpdate sys >> threadDelay 20000 >> loop in loop


      restore runApp `finally` do
        Safe.drainActive env.finishMap  -- setCallback NULL on any tracked channels
        killThread pumpTid
        hFlush stdout
        freeHaskellFunPtr cb

----------------------------------------------------------------
-- Groups (FMOD)
----------------------------------------------------------------

mkGroupFmod :: EnvFMOD -> String -> IO (I.Group FmodState)
mkGroupFmod env name = do
  grp <- Safe.createChannelGroup env.system name
  modifyMVar_ env.groupMap $ \gm -> pure (Map.insert name grp gm)
  pure (I.GroupName name)

mkOrGetGroupFmod :: EnvFMOD -> String -> IO (I.Group FmodState)
mkOrGetGroupFmod env name = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Just _ -> pure (I.GroupName name)
    Nothing  -> mkGroupFmod env name

addToGroupFmod :: forall st. I.Groupable st => EnvFMOD -> I.Group FmodState -> FmodState st -> IO ()
addToGroupFmod env (I.GroupName name) s = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing -> pure ()
    Just grp -> case s of
      PlayingSound ch _ _ -> Safe.setChannelGroup ch grp
      PausedSound  ch _ _ -> Safe.setChannelGroup ch grp

removeFromGroupFmod :: forall st. I.Groupable st => EnvFMOD -> I.Group FmodState -> FmodState st -> IO ()
removeFromGroupFmod env _ s = do
  master <- Safe.getMasterChannelGroup env.system
  case s of
    PlayingSound ch _ _ -> Safe.setChannelGroup ch master
    PausedSound  ch _ _ -> Safe.setChannelGroup ch master

pauseGroupFmod :: EnvFMOD -> I.Group FmodState -> IO ()
pauseGroupFmod env (I.GroupName name) = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing  -> pure ()
    Just grp -> Safe.setGroupPaused grp True

resumeGroupFmod :: EnvFMOD -> I.Group FmodState -> IO ()
resumeGroupFmod env (I.GroupName name) = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing  -> pure ()
    Just grp -> Safe.setGroupPaused grp False

setGroupVolumeFmod :: EnvFMOD -> I.Group FmodState -> I.Volume -> IO ()
setGroupVolumeFmod env (I.GroupName name) vol = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing  -> pure ()
    Just grp -> Safe.setGroupVolume grp (realToFrac $ I.unVolume vol)

setGroupPanningFmod :: EnvFMOD -> I.Group FmodState -> I.Panning -> IO ()
setGroupPanningFmod env (I.GroupName name) pan = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing  -> pure ()
    Just grp -> Safe.setGroupPanning grp (realToFrac $ I.unPanning pan)
