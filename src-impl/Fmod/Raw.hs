{-# LANGUAGE ForeignFunctionInterface #-}

module Fmod.Raw where 

import Foreign ( Ptr, FunPtr )
import Foreign.C
    ( CUInt(..), CInt(..), CString, CBool(..), CFloat(..))

data FMODSystem
data FMODSound
data FMODChannel

type FMOD_RESULT = CInt


foreign import ccall safe "FMOD_System_Create"
  c_FMOD_System_Create :: Ptr (Ptr FMODSystem) -> CUInt -> IO FMOD_RESULT

foreign import ccall safe "FMOD_System_Init"
  c_FMOD_System_Init :: Ptr FMODSystem -> CInt -> CInt -> Ptr () -> IO FMOD_RESULT

foreign import ccall safe "FMOD_System_CreateSound"
  c_FMOD_System_CreateSound :: Ptr FMODSystem -> CString -> CUInt -> Ptr () -> Ptr (Ptr FMODSound) -> IO FMOD_RESULT

foreign import ccall safe "FMOD_System_PlaySound"
  c_FMOD_System_PlaySound :: Ptr FMODSystem -> Ptr FMODSound -> Ptr () -> CInt -> Ptr (Ptr FMODChannel) -> IO FMOD_RESULT

foreign import ccall safe "FMOD_System_Update"
  c_FMOD_System_Update :: Ptr FMODSystem -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_SetPaused"
  c_FMOD_Channel_SetPaused :: Ptr FMODChannel -> CBool -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_Stop"
  c_FMOD_Channel_Stop :: Ptr FMODChannel -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_SetLoopCount"
  c_FMOD_Channel_SetLoopCount :: Ptr FMODChannel -> CInt -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_SetMode"
  c_FMOD_Channel_SetMode :: Ptr FMODChannel -> CInt -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_SetVolume"
  c_FMOD_Channel_SetVolume :: Ptr FMODChannel -> CFloat -> IO FMOD_RESULT

foreign import ccall safe"FMOD_Channel_SetPan"
  c_FMOD_Channel_SetPan :: Ptr FMODChannel -> CFloat -> IO FMOD_RESULT

foreign import ccall safe"FMOD_Channel_IsPlaying"
  c_FMOD_Channel_IsPlaying :: Ptr FMODChannel -> Ptr CBool -> IO FMOD_RESULT

foreign import ccall safe "FMOD_Channel_GetVolume"
  c_FMOD_Channel_GetVolume :: Ptr FMODChannel -> Ptr CFloat -> IO FMOD_RESULT

foreign import ccall safe "fmod_create_sound_from_memory"
  c_fmod_create_sound_from_memory :: Ptr FMODSystem -> Ptr () -> CInt -> Ptr (Ptr FMODSound) -> IO FMOD_RESULT


