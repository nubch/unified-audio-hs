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

-- stopFmod :: SystemHandle -> Channel -> IO ()
-- stopFmod _ ch = do
-- result <- c_FMOD_Channel_Stop ch
-- when (result /= 0) $ putStrLn $ "FMOD_Channel_Stop failed: " ++ show result

-- setVolumeFmod :: Channel -> Volume -> IO ()
-- setVolumeFmod ch vol = do
-- result <- c_FMOD_Channel_SetVolume ch (realToFrac $ unVolume vol)
-- when (result /= 0) $ putStrLn $ "FMOD_CHANNEL_VOLUME FAILED " ++ show result

-- setPanningFmod :: Channel -> Panning -> IO ()
-- setPanningFmod ch pan = do
-- result <- c_FMOD_Channel_SetPan ch (realToFrac $ unPanning pan)
-- when (result /= 0) $ putStrLn $ "FMOD_CHANNEL_PANNING FAILED " ++ show result

data FmodState :: I.Status -> Type where
  LoadedSound  :: F.Sound -> FmodState I.Loaded
  PlayingSound :: F.Channel -> FmodState I.Playing
  PausedSound  :: F.Channel -> FmodState I.Paused

makeBackendFmod :: F.System -> I.AudioBackend FmodState
makeBackendFmod sys =
  I.AudioBackend
    { I.playA = playFmod sys,
      I.loadA = loadFmod sys,
      I.pauseA = pauseFmod,
      I.resumeA = resumeFmod
    }

runAudio :: (IOE :> es) => Eff (I.Audio FmodState : es) a -> Eff es a
runAudio eff = unsafeEff_ (F.withSystem \sys -> do
    let backend = makeBackendFmod sys
    return $ evalStaticRep (I.AudioRep backend) eff) >>= id
