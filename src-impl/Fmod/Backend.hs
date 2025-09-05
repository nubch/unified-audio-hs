{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
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

import Control.Exception (bracket)
import Control.Concurrent.MVar
import Fmod.Safe (setLoopCount)

data EnvFMOD = EnvFMOD
  { system    :: Safe.System
  , finishMap :: Safe.FinishMap
  , callback  :: FunPtr Safe.ChannelCB
  }

loadFmod :: EnvFMOD -> FilePath -> IO (FmodState I.Loaded)
loadFmod env path = LoadedSound <$> Safe.createSound env.system path

updateFmod :: EnvFMOD -> FmodState I.Loaded -> IO ()
updateFmod env (LoadedSound sound) = Safe.systemUpdate env.system sound

playFmod :: EnvFMOD -> FmodState I.Loaded -> I.Times -> IO (FmodState I.Playing)
playFmod env (LoadedSound sound) times = do
  channel <- Safe.playSound env.system sound
  finished <- newEmptyMVar
  Safe.setChannelCallback channel env.callback
  putStrLn "Set callback on channel"
  Safe.withChannelPtr channel $ \pCh ->
    modifyMVar_ env.finishMap (pure . Map.insert pCh finished)
  paused <- pauseFmod (PlayingSound channel finished)
  putStrLn "Paused immediately after play"
  applyTimes times paused
  res <- resumeFmod paused
  putStrLn "Resumed playback"
  pure res

applyTimes :: I.Times -> FmodState I.Paused -> IO ()
applyTimes t (PausedSound ch _) = case t of
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
pauseFmod (PlayingSound channel finished) = do
  ch <- setPausedFmod True channel
  pure (PausedSound ch finished)

resumeFmod :: FmodState I.Paused -> IO (FmodState I.Playing)
resumeFmod (PausedSound channel finished) = do
  ch <- setPausedFmod False channel
  pure (PlayingSound ch finished)

setVolumeFmod :: FmodState I.Playing -> I.Volume -> IO ()
setVolumeFmod (PlayingSound playing _) volume = Safe.setVolume playing (realToFrac $ I.unVolume volume)

setPanningFmod :: FmodState I.Playing -> I.Panning -> IO ()
setPanningFmod (PlayingSound playing _) panning = Safe.setPanning playing (realToFrac $ I.unPanning panning)

stopChannelFmod :: EnvFMOD -> FmodState I.Playing -> IO (FmodState I.Stopped)
stopChannelFmod env (PlayingSound ch done) = do
  Safe.withChannelPtr ch $ \pCh ->
    modifyMVar_ env.finishMap (pure . Map.delete pCh)
  _ <- tryPutMVar done ()
  Safe.stopChannel ch
  pure (StoppedSound ch)

hasFinishedFmod :: FmodState I.Playing -> IO Bool
hasFinishedFmod (PlayingSound _ finished) = do
  fin <- isEmptyMVar finished
  pure $ not fin

data FmodState :: I.Status -> Type where
  LoadedSound  :: Safe.Sound -> FmodState I.Loaded
  PlayingSound :: Safe.Channel -> MVar () -> FmodState I.Playing
  PausedSound  :: Safe.Channel -> MVar () -> FmodState I.Paused
  StoppedSound :: Safe.Channel -> FmodState I.Stopped

makeBackendFmod :: EnvFMOD -> I.AudioBackend FmodState
makeBackendFmod env =
  I.AudioBackend
    { I.playA        = playFmod env,
      I.loadA        = loadFmod env,
      I.pauseA       = pauseFmod,
      I.resumeA      = resumeFmod,
      I.setVolumeA   = setVolumeFmod,
      I.setPanningA  = setPanningFmod,
      I.stopChannelA = stopChannelFmod env,
      I.hasFinishedA = hasFinishedFmod
    }

runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio eff =
  withEffToIO \runInIO ->
    -- keep callback alive for entire FMOD session
    bracket Safe.setupFMODFinished (\(_, cb) -> freeHaskellFunPtr cb) \(finMap, cb) ->
      Safe.withSystem \sys -> do
        let env     = EnvFMOD sys finMap cb
            backend = makeBackendFmod env
        runInIO (evalStaticRep (I.AudioRep backend) eff)