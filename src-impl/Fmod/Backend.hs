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

loadFmod :: F.System -> FilePath -> IO (FmodState I.Loaded)
loadFmod system path = LoadedSound <$> F.createSound system path

playFmod :: F.System -> FmodState I.Loaded -> IO (FmodState I.Playing)
playFmod system (LoadedSound sound) = PlayingSound <$> F.playSound system sound

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

stopChannel :: FmodState I.Playing -> IO (FmodState I.Stopped)
stopChannel (PlayingSound channel) = do
  F.stopChannel channel 
  return $ StoppedSound channel

data FmodState :: I.Status -> Type where
  LoadedSound  :: F.Sound -> FmodState I.Loaded
  PlayingSound :: F.Channel -> FmodState I.Playing
  PausedSound  :: F.Channel -> FmodState I.Paused
  StoppedSound :: F.Channel -> FmodState I.Stopped

makeBackendFmod :: F.System -> I.AudioBackend FmodState
makeBackendFmod sys =
  I.AudioBackend
    { I.playA = playFmod sys,
      I.loadA = loadFmod sys,
      I.pauseA = pauseFmod,
      I.resumeA = resumeFmod,
      I.setVolumeA = setVolumeFmod,
      I.setPanningA = setPanningFmod,
      I.stopChannelA = stopChannel
    }

runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio eff = unsafeEff_ (F.withSystem \sys -> do
    let backend = makeBackendFmod sys
    return $ evalStaticRep (I.AudioRep backend) eff) >>= id