{-# LANGUAGE ForeignFunctionInterface #-}
module Fmod.Safe where

import qualified Fmod.Raw as Raw
import Fmod.Result ( checkResult, FmodResult(..) )
import Foreign
    (
      alloca,
      nullFunPtr,
      Ptr,
      fromBool,
      newForeignPtr_,
      withForeignPtr,
      nullPtr,
      newForeignPtr,
      Storable(peek),
      FinalizerPtr,
      ForeignPtr,
      castPtr,
      FunPtr,
      finalizeForeignPtr)

import Foreign.C ( CFloat, CInt(..), CUInt, withCString )
import Control.Exception ( bracket )
import Control.Monad ((<=<), when)

import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map

newtype System    = System  (ForeignPtr Raw.FMODSystem)
newtype Sound     = Sound   (ForeignPtr Raw.FMODSound)
newtype Channel   = Channel   (ForeignPtr Raw.FMODChannel)

version :: CUInt
version = 0x00020221

data LoopMode = LoopOff | LoopNormal

toCuInt :: LoopMode -> CInt
toCuInt LoopOff   = 0x00000001
toCuInt LoopNormal = 0x00000002

type ChannelCB =
  Ptr () -> -- channel that triggers (PTR () here so we dont have to define channelControl)
  CInt   -> -- callback type (see FMOD_CHANNEL_CALLBACKTYPE)
  CInt   -> -- command data (callback specific)
  Ptr () ->
  Ptr () ->
  IO CInt

foreign import ccall "wrapper"
  mkChannelCallback :: ChannelCB -> IO (FunPtr ChannelCB)

type FinishMap = MVar (Map.Map (Ptr Raw.FMODChannel) (MVar ()))

setupFMODFinished :: IO (FinishMap, FunPtr ChannelCB)
setupFMODFinished = do
  finishMap <- newMVar Map.empty
  putStrLn "Created finish map"
  callback  <- mkChannelCallback $ \channelControl _controlType callbackType _ _-> do
    putStrLn "Je suis finished"  -- FMOD_CHANNEL_CALLBACKTYPE_END
    when (callbackType /= 0) $ do
      putStrLn "Je suis finished"  -- FMOD_CHANNEL_CALLBACKTYPE_END
      let pChannel = castPtr channelControl :: Ptr Raw.FMODChannel
      modifyMVar_ finishMap $ \finMap ->
        case Map.lookup pChannel finMap of
          Just finishedVar -> do
            _ <- tryPutMVar finishedVar ()
            pure (Map.delete pChannel finMap)
          Nothing   -> pure finMap
    pure 0
  pure (finishMap, callback)

drainActive :: FinishMap -> IO ()
drainActive fm = do
  snapshot <- swapMVar fm Map.empty
  -- iterate WITHOUT holding fm, so callbacks won't deadlock
  mapM_ (\(pCh, done) -> do
           -- detach the per-channel callback (belt & suspenders)
           _ <- c_FMOD_Channel_SetCallback pCh nullFunPtr
           -- unblock any waiter (idempotent)
           _ <- tryPutMVar done ()
           -- stop the channel
           checkResult =<< Raw.c_FMOD_Channel_Stop pCh
        ) (Map.toList snapshot)

setChannelCallback :: Channel -> FunPtr ChannelCB -> IO ()
setChannelCallback (Channel ch) cb = do
  withForeignPtr ch $ \pCh ->
    checkResult =<< c_FMOD_Channel_SetCallback pCh cb

withSystem :: (System -> IO a) -> IO a
withSystem = bracket acquire release
  where
    acquire = alloca $ \pSystem -> do
      checkResult =<< Raw.c_FMOD_System_Create pSystem version
      system <- peek pSystem
      checkResult =<< Raw.c_FMOD_System_Init system 512 0 nullPtr
      fp <- newForeignPtr c_FMOD_System_Release system
      pure (System fp)
    release (System fp) = finalizeForeignPtr fp   -- << run Release deterministically

createSound :: System -> FilePath -> IO Sound
createSound (System sys) path =
  withForeignPtr sys $ \pSys ->
  withCString path   $ \cPath ->
  alloca             $ \allocSound -> do
    checkResult =<< Raw.c_FMOD_System_CreateSound pSys cPath 0 nullPtr allocSound
    sndPtr <- peek allocSound
    fp     <- newForeignPtr c_FMOD_Sound_Release sndPtr
    return (Sound fp)

playSound :: System -> Sound -> IO Channel
playSound (System sys) (Sound sound) =
  withForeignPtr sys   $ \pSys ->
  withForeignPtr sound $ \pSound ->
  alloca               $ \allocChan -> do
    checkResult =<< Raw.c_FMOD_System_PlaySound pSys pSound nullPtr 0 allocChan
    chPtr <- peek allocChan
    fp    <- newForeignPtr_ chPtr
    return (Channel fp)

setPaused :: Bool -> Channel -> IO ()
setPaused paused (Channel ch) =
  withForeignPtr ch $ \pCh ->
    checkResult =<< Raw.c_FMOD_Channel_SetPaused pCh (fromBool paused)

setVolume :: Channel -> CFloat -> IO ()
setVolume (Channel channel) volume =
  withForeignPtr channel $ \pChannel ->
    checkResult =<< Raw.c_FMOD_Channel_SetVolume pChannel volume

systemUpdate :: System -> Sound -> IO ()
systemUpdate (System sys) (Sound _)=
  withForeignPtr sys (checkResult <=< Raw.c_FMOD_System_Update)

setPanning :: Channel -> CFloat -> IO ()
setPanning (Channel channel) panning =
  withForeignPtr channel $ \pChannel ->
    checkResult =<< Raw.c_FMOD_Channel_SetPan pChannel panning

stopChannel :: Channel -> IO ()
stopChannel (Channel channel) =
  withForeignPtr channel (checkResult <=< Raw.c_FMOD_Channel_Stop)

setLoopCount :: Channel -> Int -> IO ()
setLoopCount (Channel channel) times =
  withForeignPtr channel $ \pChannel ->
    checkResult =<< Raw.c_FMOD_Channel_SetLoopCount pChannel (fromIntegral times)

setChannelMode :: Channel -> LoopMode -> IO ()
setChannelMode (Channel channel) mode =
  withForeignPtr channel $ \pChannel ->
    checkResult =<< Raw.c_FMOD_Channel_SetMode pChannel (toCuInt mode)

isPlaying :: Channel -> IO Bool
isPlaying (Channel channel) =
  withForeignPtr channel $ \pChannel -> do
    alloca $ \pResult -> do
      checkResult =<< Raw.c_FMOD_Channel_IsPlaying pChannel pResult
      result <- peek pResult
      return $ toBoolean result
        where toBoolean = (/= 0)

withChannelPtr :: Channel -> (Ptr Raw.FMODChannel -> IO a) -> IO a
withChannelPtr (Channel fp) = withForeignPtr fp

foreign import ccall safe "&FMOD_System_Release"
  c_FMOD_System_Release :: FinalizerPtr Raw.FMODSystem

foreign import ccall safe "&FMOD_Sound_Release"
  c_FMOD_Sound_Release :: FinalizerPtr Raw.FMODSound

foreign import ccall safe "FMOD_Channel_SetCallback"
  c_FMOD_Channel_SetCallback :: Ptr Raw.FMODChannel -> FunPtr ChannelCB -> IO Raw.FMOD_RESULT