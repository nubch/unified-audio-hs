{-# LANGUAGE ForeignFunctionInterface #-}

module Fmod.Safe
  ( -- Types & callbacks
    Channel,
    ChannelCB,
    ChannelGroup,
    FinishMap,
    PanMap,
    Sound,
    System,
    LoopMode (..),
    -- Creation / setup
    setupFMODEnv,
    withSystem,
    createSound,
    createSoundFromBytes,
    playSound,
    createChannelGroup,
    getMasterChannelGroup,
    -- Channel helpers
    withChannelPtr,
    setChannelCallback,
    setChannelGroup,
    setChannelMode,
    setLoopCount,
    setPaused,
    tryStopChannel,
    finalizeSound,
    -- Volume / Placement / groups
    setVolume,
    getChannelVolume,
    setPlacement,
    setGroupPaused,
    setGroupVolume,
    setGroupPlacement,
    getGroupVolume,
    getGroupChannels,
    stopGroup,
    -- Misc
    systemUpdate,
    drainActive,
  )
where

import Control.Concurrent.MVar
  ( MVar,
    modifyMVar_,
    newMVar,
    swapMVar,
    tryPutMVar,
  )
import Control.Exception (bracket)
import Control.Monad (forM, when, (<=<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (ByteString (..))
import qualified Data.Map.Strict as Map
import qualified Fmod.Raw as Raw
import Fmod.Result (checkResult, invalidHandleOrOk)
import Foreign
  ( FinalizerPtr,
    ForeignPtr,
    FunPtr,
    Ptr,
    Storable (peek),
    alloca,
    castPtr,
    finalizeForeignPtr,
    fromBool,
    newForeignPtr,
    newForeignPtr_,
    nullFunPtr,
    nullPtr,
    plusPtr,
    withForeignPtr,
  )
import Foreign.C (CFloat, CInt (..), CUInt, withCString)
import qualified UnifiedAudio.Effectful as I

newtype System = System (ForeignPtr Raw.FMODSystem)

newtype Sound = Sound (ForeignPtr Raw.FMODSound)

newtype Channel = Channel (ForeignPtr Raw.FMODChannel)

newtype ChannelGroup = ChannelGroup (ForeignPtr Raw.FMODChannelGroup)

version :: CUInt
version = 0x00020221

data LoopMode = LoopOff | LoopNormal

toCuInt :: LoopMode -> CInt
toCuInt LoopOff = 0x00000001
toCuInt LoopNormal = 0x00000002

type ChannelCB =
  Ptr () -> -- channel that triggers (PTR () here so we dont have to define channelControl)
  CInt -> -- callback type (see FMOD_CHANNEL_CALLBACKTYPE)
  CInt -> -- command data (callback specific)
  Ptr () ->
  Ptr () ->
  IO CInt

foreign import ccall "wrapper"
  mkChannelCallback :: ChannelCB -> IO (FunPtr ChannelCB)

type FinishMap = MVar (Map.Map (Ptr Raw.FMODChannel) (MVar ()))

type PanMap = MVar (Map.Map (Ptr Raw.FMODChannel) I.Placement)

setupFMODEnv :: IO (FinishMap, PanMap, FunPtr ChannelCB)
setupFMODEnv = do
  finishMap <- newMVar Map.empty
  stateMap <- newMVar Map.empty
  callback <- mkChannelCallback $ \channelControl _controlType callbackType _ _ -> do
    when (callbackType == 0) $ do
      let pChannel = castPtr channelControl :: Ptr Raw.FMODChannel
      modifyMVar_ finishMap $ \finMap ->
        case Map.lookup pChannel finMap of
          Just finishedVar -> do
            _ <- tryPutMVar finishedVar ()
            pure (Map.delete pChannel finMap)
          Nothing -> pure finMap
    pure 0
  pure (finishMap, stateMap, callback)

drainActive :: FinishMap -> IO ()
drainActive fm = do
  snapshot <- swapMVar fm Map.empty
  mapM_
    ( \(pCh, done) -> do
        _ <- tryPutMVar done ()
        detachCallbackPtr pCh
        tryStopPtr pCh
    )
    (Map.toList snapshot)

setChannelCallback :: Channel -> FunPtr ChannelCB -> IO ()
setChannelCallback (Channel ch) cb = do
  withForeignPtr ch $ \pCh ->
    checkResult "setCallback" =<< c_FMOD_Channel_SetCallback pCh cb

withSystem :: (System -> IO a) -> IO a
withSystem = bracket acquire release
  where
    acquire = alloca $ \pSystem -> do
      checkResult "createSystem" =<< Raw.c_FMOD_System_Create pSystem version
      system <- peek pSystem
      checkResult "InitSystem" =<< Raw.c_FMOD_System_Init system 512 0 nullPtr
      fp <- newForeignPtr c_FMOD_System_Release system
      pure (System fp)
    release (System fp) = do
      finalizeForeignPtr fp -- << run Release deterministically

createSound :: System -> FilePath -> IO Sound
createSound (System sys) path =
  withForeignPtr sys $ \pSys ->
    withCString path $ \cPath ->
      alloca $ \allocSound -> do
        checkResult "CreateSound" =<< Raw.c_FMOD_System_CreateSound pSys cPath 0 nullPtr allocSound
        sndPtr <- peek allocSound
        fp <- newForeignPtr c_FMOD_Sound_Release sndPtr
        return (Sound fp)

playSound :: System -> Sound -> IO Channel
playSound (System sys) (Sound sound) =
  withForeignPtr sys $ \pSys ->
    withForeignPtr sound $ \pSound ->
      alloca $ \allocChan -> do
        checkResult "playSound" =<< Raw.c_FMOD_System_PlaySound pSys pSound nullPtr 0 allocChan
        chPtr <- peek allocChan
        fp <- newForeignPtr_ chPtr
        return (Channel fp)

-- ChannelGroup: creation and control

createChannelGroup :: System -> String -> IO ChannelGroup
createChannelGroup (System sys) name =
  withForeignPtr sys $ \pSys ->
    withCString name $ \cName ->
      alloca $ \allocGrp -> do
        checkResult "CreateChannelGroup" =<< Raw.c_FMOD_System_CreateChannelGroup pSys cName allocGrp
        grpPtr <- peek allocGrp
        fp <- newForeignPtr c_FMOD_ChannelGroup_Release grpPtr
        pure (ChannelGroup fp)

getMasterChannelGroup :: System -> IO ChannelGroup
getMasterChannelGroup (System sys) =
  withForeignPtr sys $ \pSys ->
    alloca $ \allocGrp -> do
      checkResult "GetMasterChannelGroup" =<< Raw.c_FMOD_System_GetMasterChannelGroup pSys allocGrp
      grpPtr <- peek allocGrp
      fp <- newForeignPtr c_FMOD_ChannelGroup_Release grpPtr
      pure (ChannelGroup fp)

setChannelGroup :: Channel -> ChannelGroup -> IO ()
setChannelGroup (Channel ch) (ChannelGroup grp) =
  withForeignPtr ch $ \pCh ->
    withForeignPtr
      grp
      ( checkResult "Channel_SetChannelGroup"
          <=< Raw.c_FMOD_Channel_SetChannelGroup pCh
      )

setGroupPaused :: ChannelGroup -> Bool -> IO ()
setGroupPaused (ChannelGroup grp) paused =
  withForeignPtr grp $ \pGrp ->
    checkResult "ChannelGroup_SetPaused" =<< Raw.c_FMOD_ChannelGroup_SetPaused pGrp (fromBool paused)

setGroupVolume :: ChannelGroup -> CFloat -> IO ()
setGroupVolume (ChannelGroup grp) vol =
  withForeignPtr grp $ \pGrp ->
    checkResult "ChannelGroup_SetVolume" =<< Raw.c_FMOD_ChannelGroup_SetVolume pGrp vol

getGroupVolume :: ChannelGroup -> IO Float
getGroupVolume (ChannelGroup grp) =
  withForeignPtr grp $ \pGrp ->
    alloca $ \pOut -> do
      checkResult "ChannelGroup_GetVolume" =<< Raw.c_FMOD_ChannelGroup_GetVolume pGrp pOut
      realToFrac <$> peek pOut

-- Enumerate current channels in a group
getGroupChannels :: ChannelGroup -> IO [Channel]
getGroupChannels (ChannelGroup grp) =
  withForeignPtr grp $ \pGrp ->
    alloca $ \pNum -> do
      checkResult "ChannelGroup_GetNumChannels" =<< Raw.c_FMOD_ChannelGroup_GetNumChannels pGrp pNum
      num <- peek pNum
      let n = max 0 (fromIntegral num)
      forM [0 .. n - 1] $ \i ->
        alloca $ \pCh -> do
          checkResult "ChannelGroup_GetChannel" =<< Raw.c_FMOD_ChannelGroup_GetChannel pGrp (fromIntegral i) pCh
          chPtr <- peek pCh
          fp <- newForeignPtr_ chPtr
          pure (Channel fp)

stopGroup :: ChannelGroup -> IO ()
stopGroup (ChannelGroup grp) =
  withForeignPtr grp (checkResult "ChannelGroup_Stop" <=< Raw.c_FMOD_ChannelGroup_Stop)

setGroupPlacement :: ChannelGroup -> CFloat -> IO ()
setGroupPlacement (ChannelGroup grp) pan =
  withForeignPtr grp $ \pGrp ->
    checkResult "ChannelGroup_SetPan" =<< Raw.c_FMOD_ChannelGroup_SetPan pGrp pan

tryStopChannel :: Channel -> IO ()
tryStopChannel (Channel ch) =
  withForeignPtr ch $ \pCh -> do
    invalidHandleOrOk "tryStop" =<< Raw.c_FMOD_Channel_Stop pCh

detachCallbackPtr :: Ptr Raw.FMODChannel -> IO ()
detachCallbackPtr pCh = do
  invalidHandleOrOk "detachCallbackPtr" =<< c_FMOD_Channel_SetCallback pCh nullFunPtr

tryStopPtr :: Ptr Raw.FMODChannel -> IO ()
tryStopPtr pCh = do
  invalidHandleOrOk "tryStopPtr" =<< Raw.c_FMOD_Channel_Stop pCh

setPaused :: Bool -> Channel -> IO ()
setPaused paused (Channel ch) =
  withForeignPtr ch $ \pCh ->
    checkResult "setPaused" =<< Raw.c_FMOD_Channel_SetPaused pCh (fromBool paused)

setVolume :: Channel -> CFloat -> IO ()
setVolume (Channel channel) volume =
  withForeignPtr channel $ \pChannel ->
    checkResult "setVolume" =<< Raw.c_FMOD_Channel_SetVolume pChannel volume

systemUpdate :: System -> IO ()
systemUpdate (System sys) =
  withForeignPtr sys (checkResult "sysUpdate" <=< Raw.c_FMOD_System_Update)

setPlacement :: Channel -> CFloat -> IO ()
setPlacement (Channel channel) placement =
  withForeignPtr channel $ \pChannel ->
    checkResult "setPlacement" =<< Raw.c_FMOD_Channel_SetPan pChannel placement

getChannelVolume :: Channel -> IO Float
getChannelVolume (Channel ch) =
  withForeignPtr ch $ \pCh ->
    alloca $ \pOut -> do
      checkResult "getChannelVolume" =<< Raw.c_FMOD_Channel_GetVolume pCh pOut
      realToFrac <$> peek pOut

setLoopCount :: Channel -> Int -> IO ()
setLoopCount (Channel channel) times =
  withForeignPtr channel $ \pChannel ->
    checkResult "setLoop" =<< Raw.c_FMOD_Channel_SetLoopCount pChannel (fromIntegral times)

setChannelMode :: Channel -> LoopMode -> IO ()
setChannelMode (Channel channel) mode =
  withForeignPtr channel $ \pChannel ->
    checkResult "setChannelMode" =<< Raw.c_FMOD_Channel_SetMode pChannel (toCuInt mode)

withChannelPtr :: Channel -> (Ptr Raw.FMODChannel -> IO a) -> IO a
withChannelPtr (Channel fp) =
  withForeignPtr fp

finalizeSound :: Sound -> IO ()
finalizeSound (Sound sound) =
  finalizeForeignPtr sound

createSoundFromBytes :: System -> BS.ByteString -> IO Sound
createSoundFromBytes (System sys) bs =
  withForeignPtr sys $ \pSys ->
    alloca $ \allocSound ->
      case bs of
        BS.PS fptr off len ->
          withForeignPtr fptr $ \base -> do
            let pData = castPtr (base `plusPtr` off)
            checkResult "createSoundBytes" =<< Raw.c_fmod_create_sound_from_memory pSys pData (fromIntegral len) allocSound
            sndPtr <- peek allocSound
            fp <- newForeignPtr c_FMOD_Sound_Release sndPtr
            pure (Sound fp)

foreign import ccall safe "&FMOD_System_Release"
  c_FMOD_System_Release :: FinalizerPtr Raw.FMODSystem

foreign import ccall safe "&FMOD_Sound_Release"
  c_FMOD_Sound_Release :: FinalizerPtr Raw.FMODSound

foreign import ccall safe "FMOD_Channel_SetCallback"
  c_FMOD_Channel_SetCallback :: Ptr Raw.FMODChannel -> FunPtr ChannelCB -> IO Raw.FMOD_RESULT

foreign import ccall safe "&FMOD_ChannelGroup_Release"
  c_FMOD_ChannelGroup_Release :: FinalizerPtr Raw.FMODChannelGroup
