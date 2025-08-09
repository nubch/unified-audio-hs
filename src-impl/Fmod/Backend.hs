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
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static
  ( evalStaticRep,
    unsafeEff_,
  )

-- Interface
import qualified UnifiedAudio.Effectful as I
import qualified Fmod.Safe as F
import Fmod.Safe (setLoopCount)

loadFmod :: F.System -> FilePath -> IO (FmodState I.Loaded)
loadFmod system path = LoadedSound <$> F.createSound system path

playFmod :: F.System -> FmodState I.Loaded -> I.Times -> IO (FmodState I.Playing)
playFmod system (LoadedSound sound) times = do
  channel <- F.playSound system sound
  paused <- pauseFmod (PlayingSound channel)
  applyTimes times paused
  resumeFmod paused

applyTimes :: I.Times -> FmodState I.Paused -> IO ()
applyTimes t (PausedSound ch) = case t of
  I.Once -> do
    F.setChannelMode ch F.LoopOff
    setLoopCount ch 0
  I.Times n -> do
    F.setChannelMode ch F.LoopNormal
    setLoopCount ch (n - 1)
  I.Forever -> do
    F.setChannelMode ch F.LoopNormal
    setLoopCount ch (-1)

setPausedFmod :: Bool -> F.Channel -> IO F.Channel
setPausedFmod paused channel = F.setPaused paused channel >> return channel

pauseFmod :: FmodState I.Playing -> IO (FmodState I.Paused)
pauseFmod (PlayingSound channel) = PausedSound <$> setPausedFmod True channel

resumeFmod :: FmodState I.Paused -> IO (FmodState I.Playing)
resumeFmod (PausedSound channel) = PlayingSound <$> setPausedFmod False channel

setVolumeFmod :: FmodState I.Playing -> I.Volume -> IO ()
setVolumeFmod (PlayingSound playing) volume = F.setVolume playing (realToFrac $ I.unVolume volume)

setPanningFmod :: FmodState I.Playing -> I.Panning -> IO ()
setPanningFmod (PlayingSound playing) panning = F.setPanning playing (realToFrac $ I.unPanning panning)

stopChannelFmod :: FmodState I.Playing -> IO (FmodState I.Stopped)
stopChannelFmod (PlayingSound channel) =
  F.stopChannel channel >> return (StoppedSound channel)

isPlayingFmod :: FmodState I.Playing -> IO Bool
isPlayingFmod (PlayingSound channel) = F.isPlaying channel

data FmodState :: I.Status -> Type where
  LoadedSound  :: F.Sound -> FmodState I.Loaded
  PlayingSound :: F.Channel -> FmodState I.Playing
  PausedSound  :: F.Channel -> FmodState I.Paused
  StoppedSound :: F.Channel -> FmodState I.Stopped

makeBackendFmod :: F.System -> I.AudioBackend FmodState
makeBackendFmod sys =
  I.AudioBackend
    { I.playA        = playFmod sys,
      I.loadA        = loadFmod sys,
      I.pauseA       = pauseFmod,
      I.resumeA      = resumeFmod,
      I.setVolumeA   = setVolumeFmod,
      I.setPanningA  = setPanningFmod,
      I.stopChannelA = stopChannelFmod,
      I.isPlayingA   = isPlayingFmod

    }

runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio eff = unsafeEff_ (F.withSystem \sys -> do
    let backend = makeBackendFmod sys
    return $ evalStaticRep (I.AudioRep backend) eff) >>= id