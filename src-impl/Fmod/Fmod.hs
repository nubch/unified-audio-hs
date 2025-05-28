--{-# LANGUAGE ForeignFunctionInterface #-}

module Fmod.Fmod where

-- import Foreign ( Ptr, alloca, nullPtr, Storable(peek) )
-- import Foreign.C.Types ( CInt(..), CUInt(..) )
-- import Control.Monad ( when )
-- import Foreign.C.String ( withCString, CString )
-- import Interface ( AudioEffect, AudioEffect(..) )

-- data FMODSystem
-- data FMODSound
-- data FMODChannel

-- type SystemHandle = Ptr FMODSystem
-- type SoundHandle = Ptr FMODSound
-- type PlayingHandle = Ptr FMODChannel
-- type FMOD_RESULT = CInt

-- version :: CUInt
-- version = 0x00020221

-- foreign import ccall "FMOD_System_Create"
--   c_FMOD_System_Create :: Ptr (Ptr FMODSystem) -> CUInt -> IO FMOD_RESULT

-- foreign import ccall "FMOD_System_Init"
--   c_FMOD_System_Init :: Ptr FMODSystem -> CInt -> CInt -> Ptr () -> IO FMOD_RESULT

-- foreign import ccall "FMOD_System_CreateSound"
--   c_FMOD_System_CreateSound :: Ptr FMODSystem -> CString -> CUInt -> Ptr () -> Ptr (Ptr FMODSound) -> IO CInt

-- foreign import ccall "FMOD_System_PlaySound"
--   c_FMOD_System_PlaySound :: Ptr FMODSystem -> Ptr FMODSound -> Ptr () -> CInt -> Ptr (Ptr FMODChannel) -> IO CInt

-- foreign import ccall "FMOD_Channel_Stop"
--   c_FMOD_Channel_Stop :: Ptr FMODChannel -> IO CInt

-- initFmod :: IO SystemHandle
-- initFmod = alloca $ \pSystem -> do
--   result <- c_FMOD_System_Create pSystem version
--   when (result /= 0) $ error $ "Fmod Create failed" ++ show result

--   system <- peek pSystem
--   result2 <- c_FMOD_System_Init system 512 0 nullPtr
--   if result2 /= 0 then error $ "FMOD Init failed" ++ show result2 else return system

-- loadFmod :: SystemHandle -> FilePath -> IO SoundHandle
-- loadFmod system path = withCString path $ \cPath ->
--   alloca $ \pSound -> do
--     result <- c_FMOD_System_CreateSound system cPath 0 nullPtr pSound
--     if result /= 0
--       then error $ "FMOD_CreateSound failed: " ++ show result
--       else peek pSound

-- playFmod :: SystemHandle -> SoundHandle -> IO PlayingHandle
-- playFmod system sound = alloca $ \pChannel -> do
--   result <- c_FMOD_System_PlaySound system sound nullPtr 0 pChannel
--   if result /= 0
--     then error $ "FMOD_PlaySound failed: " ++ show result
--     else peek pChannel

-- stopFmod :: SystemHandle -> PlayingHandle -> IO ()
-- stopFmod _ ch = do
--   result <- c_FMOD_Channel_Stop ch
--   when (result /= 0) $ putStrLn $ "FMOD_Channel_Stop failed: " ++ show result

-- backendFmod :: AudioEffect SystemHandle SoundHandle PlayingHandle
-- backendFmod = AudioBackend {
--     initAudioB = initFmod,
--     loadSound = loadFmod,
--     playSound = playFmod,
--     stopSound = stopFmod
-- }
