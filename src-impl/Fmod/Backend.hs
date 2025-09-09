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

-- Effectful
import Data.Kind (Type)
import Foreign ( FunPtr, freeHaskellFunPtr)
import Effectful (Eff, IOE, type (:>), withEffToIO)
import Effectful.Dispatch.Static
  ( evalStaticRep, unsafeEff_
  )

-- Interface
import qualified UnifiedAudio.Effectful as I
import qualified Fmod.Safe as Safe
import qualified Data.Map.Strict as Map

import Control.Exception ( mask, finally )
import Control.Concurrent.MVar
import Fmod.Safe (setLoopCount)
import System.IO (hFlush, stdout)

data EnvFMOD = EnvFMOD
  { system    :: Safe.System
  , finishMap :: Safe.FinishMap
  , callback  :: FunPtr Safe.ChannelCB
  }

loadFmod :: EnvFMOD -> I.Source -> I.SoundType -> IO (FmodState I.Loaded)
loadFmod env src _ = case src of
  I.FromFile fp ->
    LoadedSound <$> Safe.createSound env.system fp
  I.FromBytes by ->
    LoadedSound <$> Safe.createSoundFromBytes env.system by

unloadFmod :: FmodState I.Loaded -> IO ()
unloadFmod (LoadedSound sound) = do
  Safe.finalizeSound sound

updateFmod :: EnvFMOD -> FmodState I.Loaded -> IO ()
updateFmod env (LoadedSound sound) = Safe.systemUpdate env.system sound

playFmod :: EnvFMOD -> FmodState I.Loaded -> I.Times -> IO (FmodState I.Playing)
playFmod env (LoadedSound sound) times = do
  channel <- Safe.playSound env.system sound
  finished <- newEmptyMVar
  Safe.setChannelCallback channel env.callback
  Safe.withChannelPtr channel $ \pCh ->
    modifyMVar_ env.finishMap (pure . Map.insert pCh finished)
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

setVolumeFmod :: forall adj. I.Adjustable adj => FmodState adj -> I.Volume -> IO ()
setVolumeFmod adjustable volume =
  case adjustable of
    (PlayingSound playing _ _) -> setVolume playing volume
    (PausedSound  playing _ _) -> setVolume playing volume
  where
    setVolume ch vol = Safe.setVolume ch (realToFrac $ I.unVolume vol)

setPanningFmod :: forall adj. I.Adjustable adj => FmodState adj -> I.Panning -> IO ()
setPanningFmod adjustable panning =
  case adjustable of
    (PlayingSound playing _ _) -> setPanning playing panning
    (PausedSound  playing _ _) -> setPanning playing panning
  where
    setPanning ch pan =  Safe.setPanning ch (realToFrac $ I.unPanning pan)

stopChannelFmod :: forall st. I.Stoppable st => EnvFMOD -> FmodState st -> IO (FmodState I.Stopped)
stopChannelFmod env stoppable = do
  case stoppable of
    (PlayingSound channel finished _) -> stop channel finished
    (PausedSound  channel finished _) -> stop channel finished
  where
    stop ch done = do
      Safe.withChannelPtr ch $ \pCh ->
        modifyMVar_ env.finishMap (pure . Map.delete pCh)
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

data FmodState :: I.Status -> Type where
  LoadedSound  :: Safe.Sound -> FmodState I.Loaded
  PlayingSound :: Safe.Channel -> MVar () -> Safe.Sound -> FmodState I.Playing
  PausedSound  :: Safe.Channel -> MVar () -> Safe.Sound -> FmodState I.Paused
  StoppedSound :: Safe.Channel -> FmodState I.Stopped

makeBackendFmod :: EnvFMOD -> I.AudioBackend FmodState
makeBackendFmod env =
  I.AudioBackend
    { I.playA          = playFmod env,
      I.loadA          = loadFmod env,
      I.pauseA         = pauseFmod,
      I.resumeA        = resumeFmod,
      I.setVolumeA     = setVolumeFmod,
      I.setPanningA    = setPanningFmod,
      I.stopChannelA   = stopChannelFmod env,
      I.hasFinishedA   = hasFinishedFmod,
      I.unloadA        = unloadFmod,
      I.awaitFinishedA = awaitFinishedFmod
    }

runAudio
  :: (IOE :> es)
  => Eff (I.Audio FmodState : es) a
  -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO ->
    -- All FMOD lifetime in one scope:
    Safe.withSystem $ \sys -> mask $ \restore -> do
      -- allocate finished-callback, keep alive for whole session
      (finMap, cb) <- Safe.setupFMODFinished

      let env     = EnvFMOD sys finMap cb
          backend = makeBackendFmod env
          runApp  = runInIO (evalStaticRep (I.AudioRep backend) eff)

      -- run the app; on any exit, detach+free then log
      restore runApp `finally` do
        -- IMPORTANT: detach callbacks BEFORE closing/releasing FMOD
        --Safe.drainActive env.finishMap  -- setCallback NULL on any tracked channels
        hFlush stdout
        freeHaskellFunPtr cb