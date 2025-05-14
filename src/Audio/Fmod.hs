{-# LANGUAGE ForeignFunctionInterface #-}

module Audio.Fmod where

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString, peekCString)

-- Abstract pointer to the FMOD_System
data FMODSystem

type FMOD_RESULT = CInt

foreign import ccall "FMOD_System_Create"
  c_FMOD_System_Create :: Ptr (Ptr FMODSystem) -> IO FMOD_RESULT

foreign import ccall "FMOD_System_Init"
  c_FMOD_System_Init :: Ptr FMODSystem -> CInt -> CInt -> Ptr () -> IO FMOD_RESULT

foreign import ccall "FMOD_ErrorString"
  c_FMOD_ErrorString :: CInt -> IO CString

printError :: CInt -> IO ()
printError code = do
  msgPtr <- c_FMOD_ErrorString code
  msg <- peekCString msgPtr
  putStrLn $ "FMOD error " ++ show code ++ ": " ++ msg