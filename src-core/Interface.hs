{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Interface
  ( AudioEffect(..),
    AudioBackend(..)
  ) where

import Effectful
import Effectful.Dispatch.Static (SideEffects (..), getStaticRep, unsafeEff_, StaticRep, evalStaticRep)

type SystemHandle = ()
type SoundHandle = String
type PlayingHandle = Int

data AudioEffect :: Effect where
    InitAudio   :: AudioEffect m SystemHandle
    LoadSound   :: SystemHandle -> FilePath -> AudioEffect m SoundHandle
    PlaySound   :: SystemHandle -> SoundHandle -> AudioEffect m PlayingHandle
    StopSound   :: SystemHandle -> PlayingHandle -> AudioEffect m ()
  
type instance DispatchOf AudioEffect = Static WithSideEffects

data AudioBackend = AudioBackend
  { initAudioB :: IO SystemHandle
  , loadSoundB :: SystemHandle -> FilePath -> IO SoundHandle
  , playSoundB :: SystemHandle -> SoundHandle -> IO PlayingHandle
  , stopSoundB :: SystemHandle -> PlayingHandle -> IO ()
  }

newtype instance StaticRep AudioEffect = AudioRep AudioBackend

-- Smart-Constructors
initAudio :: (AudioEffect :> es) => Eff es SystemHandle
initAudio = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ initAudioB backend

loadSound :: (AudioEffect :> es) => SystemHandle -> FilePath -> Eff es SoundHandle
loadSound sys fp = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ loadSoundB backend sys fp

playSound :: (AudioEffect :> es) => SystemHandle -> SoundHandle -> Eff es PlayingHandle
playSound sys sound = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ playSoundB backend sys sound

stopSound :: (AudioEffect :> es) => SystemHandle -> PlayingHandle -> Eff es ()
stopSound sys playing = do
  AudioRep backend <- getStaticRep
  unsafeEff_ $ stopSoundB backend sys playing

runAudio :: (IOE :> es) => AudioBackend -> Eff (AudioEffect : es) a -> Eff es a
runAudio backend = evalStaticRep (AudioRep backend)