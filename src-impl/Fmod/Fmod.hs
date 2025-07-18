{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Fmod.Fmod (runAudio, Channel) where

-- Effectful
import Data.Kind (Type)
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static
  ( evalStaticRep,
    unsafeEff_,
  )

-- FFI
import Foreign (Ptr, Storable (peek), alloca, nullPtr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CUInt (..))
import Foreign.Marshal.Utils (fromBool)

-- Interface
import Interface
import Fmod.Result (checkResult)

data FMODSystem

data FMODSound

data FMODChannel

type SystemHandle = Ptr FMODSystem

type SoundHandle = Ptr FMODSound

type Channel = Ptr FMODChannel

type FMOD_RESULT = CInt

version :: CUInt
version = 0x00020221

foreign import ccall "FMOD_System_Create"
  c_FMOD_System_Create :: Ptr (Ptr FMODSystem) -> CUInt -> IO FMOD_RESULT

foreign import ccall "FMOD_System_Init"
  c_FMOD_System_Init :: Ptr FMODSystem -> CInt -> CInt -> Ptr () -> IO FMOD_RESULT

foreign import ccall "FMOD_System_CreateSound"
  c_FMOD_System_CreateSound :: Ptr FMODSystem -> CString -> CUInt -> Ptr () -> Ptr (Ptr FMODSound) -> IO FMOD_RESULT

foreign import ccall "FMOD_System_PlaySound"
  c_FMOD_System_PlaySound :: Ptr FMODSystem -> Ptr FMODSound -> Ptr () -> CInt -> Ptr (Ptr FMODChannel) -> IO FMOD_RESULT

foreign import ccall "FMOD_Channel_SetPaused"
  c_FMOD_Channel_SetPaused :: Ptr FMODChannel -> CBool -> IO FMOD_RESULT

foreign import ccall "FMOD_Channel_Stop"
  c_FMOD_Channel_Stop :: Ptr FMODChannel -> IO FMOD_RESULT

foreign import ccall "FMOD_Channel_SetVolume"
  c_FMOD_Channel_SetVolume :: Ptr FMODChannel -> CFloat -> IO FMOD_RESULT

foreign import ccall "FMOD_Channel_SetPan"
  c_FMOD_Channel_SetPan :: Ptr FMODChannel -> CFloat -> IO FMOD_RESULT

initFmod :: IO SystemHandle
initFmod = alloca $ \pSystem -> do
  checkResult =<< c_FMOD_System_Create pSystem version
  system <- peek pSystem
  checkResult =<< c_FMOD_System_Init system 512 0 nullPtr
  return system

loadFmod :: SystemHandle -> FilePath -> IO (FmodState Loaded)
loadFmod system path = withCString path $ \cPath ->
  alloca $ \pSound -> do
    checkResult =<< c_FMOD_System_CreateSound system cPath 0 nullPtr pSound
    LoadedSound <$> peek pSound

playFmod :: SystemHandle -> FmodState Loaded -> IO (FmodState Playing)
playFmod system (LoadedSound sound) = alloca $ \pChannel -> do
  checkResult =<< c_FMOD_System_PlaySound system sound nullPtr 0 pChannel
  PlayingSound <$> peek pChannel

setPausedFmod :: Bool -> Channel -> IO Channel
setPausedFmod paused channel = do
  checkResult =<< c_FMOD_Channel_SetPaused channel (fromBool paused)
  return channel

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
  LoadedSound :: SoundHandle -> FmodState Loaded
  PlayingSound :: Channel -> FmodState Playing
  PausedSound :: Channel -> FmodState Paused

makeBackendFmod :: SystemHandle -> AudioBackend FmodState
makeBackendFmod sys =
  AudioBackend
    { playA = playFmod sys,
      loadA = loadFmod sys,
      pauseA = pauseFmod,
      resumeA = resumeFmod
    }

runAudio :: (IOE :> es) => Eff (Audio FmodState : es) a -> Eff es a
runAudio eff = do
  sys <- unsafeEff_ initFmod
  let backend = makeBackendFmod sys
  evalStaticRep (AudioRep backend) eff