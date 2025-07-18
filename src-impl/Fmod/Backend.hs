{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}

module Fmod.Backend (runAudio, Channel) where

-- Effectful
import Data.Kind (Type)
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static
  ( evalStaticRep,
    unsafeEff_,
  )

-- Interface
import Interface
import Fmod.Raw
import qualified Fmod.Safe as F


loadFmod :: F.System -> FilePath -> IO (FmodState Loaded)
loadFmod system path = LoadedSound <$> F.createSound system path

playFmod :: F.System -> FmodState Loaded -> IO (FmodState Playing)
playFmod system (LoadedSound sound) = PlayingSound <$> F.playSound system sound

setPausedFmod :: Bool -> F.Channel -> IO F.Channel
setPausedFmod paused channel = F.setPaused paused channel >> return channel

pauseFmod :: FmodState Playing -> IO (FmodState Paused)
pauseFmod (PlayingSound channel) = PausedSound <$> setPausedFmod True channel

resumeFmod :: FmodState Paused -> IO (FmodState Playing)
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

data FmodState :: Status -> Type where
  LoadedSound  :: F.Sound -> FmodState Loaded
  PlayingSound :: F.Channel -> FmodState Playing
  PausedSound  :: F.Channel -> FmodState Paused

makeBackendFmod :: F.System -> AudioBackend FmodState
makeBackendFmod sys =
  AudioBackend
    { playA = playFmod sys,
      loadA = loadFmod sys,
      pauseA = pauseFmod,
      resumeA = resumeFmod
    }

runAudio :: (IOE :> es) => Eff (Audio FmodState : es) a -> Eff es a
runAudio eff = unsafeEff_ (F.withSystem \sys -> do
    let backend = makeBackendFmod sys
    return $ evalStaticRep (AudioRep backend) eff) >>= id
