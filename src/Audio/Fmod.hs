{-# LANGUAGE ForeignFunctionInterface #-}

module Audio.Fmod where

import Foreign
import Foreign.C.Types

-- Abstract pointer to the FMOD_System
data FMODSystem

type FMOD_RESULT = CInt

foreign import ccall "FMOD_System_Create"
  c_FMOD_System_Create :: Ptr (Ptr FMODSystem) -> IO FMOD_RESULT

foreign import ccall "FMOD_System_Init"
  c_FMOD_System_Init :: Ptr FMODSystem -> CInt -> CInt -> Ptr () -> IO FMOD_RESULT